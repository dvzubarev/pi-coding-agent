;;; pilish-org-tools.el --- Tool presentation for Org transcripts -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure presentation helpers.  Never execute tool commands, visit source files,
;; or invoke major-mode functions to identify a tool payload's language.
;; Edit diffs describe requested replacement snippets, not whole-file patches.

;;; Code:

(require 'json)
(require 'pilish-org-markdown)

(defgroup pilish-org-tools nil
  "Tool presentation in generated Org transcripts."
  :group 'org)

(defcustom pilish-org-tool-presentations
  '(("bash" :call pilish--org-tool-bash-body)
    ("read" :call pilish--org-tool-file-body :result-language path)
    ("write" :call pilish--org-tool-write-body)
    ("edit" :call pilish--org-tool-edit-body)
    ("code_outline" :call pilish--org-tool-file-body :result-language "grep")
    ("code_definition" :call pilish--org-tool-file-body :result-language path))
  "Presentation rules keyed by exact tool name, with the first match winning.
Each value is a plist supporting these keys:

:call -- function called with (CALL RESULT), the original tool-call block
and optional result entry.  Return an Org body string (empty to omit it),
or nil to fall back to JSON arguments.  Do not insert headings or mutate
records.  Escape untrusted text with `pilish--org-literal-block'.

:result-language -- Org source language string, the symbol `path' to infer
it from call arguments, or a function of (CALL RESULT) returning a language
string or nil.  CALL is nil for standalone results.  Errors always use
example blocks, without calling this function.  Output stays literal,
escaped, and non-executable.

Callbacks are trusted user configuration, never read from a session file.
Unregistered tools retain JSON arguments and literal example-block output.
Refresh the viewer after changing this option."
  :type '(alist :key-type string :value-type sexp)
  :group 'pilish-org-tools)

(defun pilish--org-tool-header-value (value)
  "Serialize tool argument VALUE as a non-executable Org header value."
  (cond
   ((or (null value) (memq value '(:null :false))) "nil")
   ((eq value t) "t")
   ((numberp value) (number-to-string value))
   (t (let ((print-escape-newlines t)
            (print-escape-control-characters t))
        (prin1-to-string
         (if (stringp value) value
           (let ((json-null :null) (json-false :false)) (json-encode value))))))))

(defun pilish--org-tool-headers (args &optional omit)
  "Render plist ARGS as Org header metadata, excluding keys in OMIT.
Babel control keys are prefixed with pi- to avoid giving tool metadata
execution, expansion, export, or file-writing semantics."
  (let (parts)
    (while args
      (let* ((key (pop args))
             (value (pop args))
             (name (and (keywordp key) (substring (symbol-name key) 1))))
        (when (and name (not (memq key omit))
                   (string-match-p "\\`[[:alnum:]_-]+\\'" name))
          (when (member name '("eval" "var" "post" "prologue" "epilogue"
                               "tangle" "file" "dir" "session" "noweb" "cache"
                               "results" "exports" "wrap" "shebang" "cmdline"))
            (setq name (concat "pi-" name)))
          (push (concat ":" name " " (pilish--org-tool-header-value value)) parts))))
    (string-join (nreverse parts) " ")))

(defun pilish--org-tool-path (args)
  "Return the file path in tool ARGS, or nil."
  (let ((path (or (plist-get args :path) (plist-get args :file_path))))
    (and (stringp path) path)))

(defun pilish--org-tool-language (path)
  "Guess an Org source language from PATH without opening a file.
Use common filename conventions, then inspect `auto-mode-alist' without
calling any mode functions.  Return nil for an unknown language."
  (when (stringp path)
    (let* ((case-fold-search t)
           (name (downcase (file-name-nondirectory path)))
           (extension (downcase (or (file-name-extension path) "")))
           (mode (assoc-default path auto-mode-alist #'string-match))
           (base (and (symbolp mode) mode
                      (string-match "\\`\\(.+?\\)\\(?:-ts\\)?-mode\\'" (symbol-name mode))
                      (match-string 1 (symbol-name mode)))))
      (or (cdr (assoc extension '(("md" . "markdown") ("mdx" . "markdown")
                                 ("json" . "json") ("jsonl" . "json")
                                 ("yaml" . "yaml") ("yml" . "yaml")
                                 ("toml" . "toml") ("ts" . "typescript")
                                 ("tsx" . "tsx") ("jsx" . "js")
                                 ("sh" . "bash") ("bash" . "bash"))))
          (cond ((string-match-p "\\`\\(?:gnu\\)?makefile\\(?:\\..*\\)?\\'" name) "makefile")
                ((string-match-p "\\`dockerfile\\(?:\\..*\\)?\\'" name) "dockerfile")
                ((member name '(".bashrc" ".bash_profile")) "bash"))
          (and base
               (not (member base '("fundamental" "text" "conf")))
               (or (car (rassoc (intern base) org-src-lang-modes)) base))))))

(defun pilish--org-tool-presentation (call result)
  "Return the configured presentation for CALL or standalone RESULT."
  (cdr (assoc (or (plist-get call :name)
                  (plist-get (plist-get result :message) :toolName))
              pilish-org-tool-presentations)))

(defun pilish--org-tool-result-language (call result)
  "Resolve RESULT's configured source language using CALL when available.
Error results always remain literal example blocks."
  (unless (eq (plist-get (plist-get result :message) :isError) t)
    (let ((rule (plist-get (pilish--org-tool-presentation call result) :result-language)))
      (cond
       ((eq rule 'path)
        (pilish--org-tool-language (pilish--org-tool-path (plist-get call :arguments))))
       ((stringp rule) rule)
       ((functionp rule) (funcall rule call result))))))

(defun pilish--org-tool-call-body (call result)
  "Return the configured Org body for CALL and RESULT, or nil for JSON."
  (when-let* ((renderer (plist-get (pilish--org-tool-presentation call result) :call)))
    (funcall renderer call result)))

(defun pilish--org-tool-bash-body (call _result)
  "Format CALL's command as a safe bash block."
  (let ((args (plist-get call :arguments)))
    (when (stringp (plist-get args :command))
      (pilish--org-literal-block
       (plist-get args :command) "bash"
       (pilish--org-tool-headers args '(:command))))))

(defun pilish--org-tool-file-body (call _result)
  "Omit CALL's arguments when its path is already shown in the heading."
  (when (pilish--org-tool-path (plist-get call :arguments)) ""))

(defun pilish--org-tool-write-body (call _result)
  "Format CALL's requested file content, not a claim of a successful write."
  (let ((args (plist-get call :arguments)))
    (when (stringp (plist-get args :content))
      (pilish--org-literal-block
       (plist-get args :content)
       (pilish--org-tool-language (pilish--org-tool-path args))
       (pilish--org-tool-headers args '(:content))))))

(defun pilish--org-tool-edit-body (call result)
  "Format CALL's requested snippets or RESULT's recorded successful diff."
  (let ((recorded-diff
         (and (not (eq (plist-get (plist-get result :message) :isError) t))
              (plist-get (plist-get (plist-get result :message) :details) :diff)))
        diffs)
    (cond
     ((and (stringp recorded-diff) (not (string-empty-p recorded-diff)))
      (concat "Recorded changes:\n\n"
              (pilish--org-literal-block recorded-diff "diff")))
     ((setq diffs (pilish--org-tool-edit-diffs (plist-get call :arguments)))
      (concat "Requested replacements (snippet-relative lines; not a whole-file patch).\n\n"
              (mapconcat (lambda (diff) (pilish--org-literal-block diff "diff"))
                         diffs "\n\n"))))))

(defun pilish--org-tool-diff-lines (text prefix)
  "Return diff lines for TEXT with PREFIX, retaining final-newline semantics."
  (unless (string-empty-p text)
    (concat (mapconcat (lambda (line) (concat prefix line))
                       (split-string (string-remove-suffix "\n" text) "\n") "\n")
            "\n"
            (unless (string-suffix-p "\n" text)
              "\\ No newline at end of file\n"))))

(defun pilish--org-tool-snippet-diff (old new path)
  "Format a unified diff of OLD and NEW replacement snippets for PATH.
Line numbers are snippet-relative, never inferred from the current file."
  (let ((old-count (if (string-empty-p old) 0
                     (length (split-string (string-remove-suffix "\n" old) "\n"))))
        (new-count (if (string-empty-p new) 0
                     (length (split-string (string-remove-suffix "\n" new) "\n")))))
    (concat "--- " (prin1-to-string (concat "a/" (or path "snippet"))) "\n"
            "+++ " (prin1-to-string (concat "b/" (or path "snippet"))) "\n"
            (format "@@ -%d,%d +%d,%d @@\n"
                    (if (zerop old-count) 0 1) old-count
                    (if (zerop new-count) 0 1) new-count)
            (pilish--org-tool-diff-lines old "-")
            (pilish--org-tool-diff-lines new "+"))))

(defun pilish--org-tool-edit-diffs (args)
  "Return requested edit snippets from ARGS as a list of unified diffs.
Support both edits arrays and legacy oldText/newText arguments.  Return
nil when any edit has an unknown shape, so callers can retain raw JSON."
  (let ((edits (if (plist-member args :edits) (plist-get args :edits)
                 (vector args)))
        (path (pilish--org-tool-path args)))
    (when (and (or (vectorp edits) (consp edits)) (> (length edits) 0)
               (seq-every-p (lambda (edit)
                              (and (stringp (plist-get edit :oldText))
                                   (stringp (plist-get edit :newText)))) edits))
      (mapcar (lambda (edit)
                (pilish--org-tool-snippet-diff
                 (plist-get edit :oldText) (plist-get edit :newText) path))
              (append edits nil)))))

(defun pilish--org-tool-pairs (entries)
  "Pair tool calls/results in selected-branch ENTRIES in chronological order.
Return (RESULTS . CONSUMED), hash tables keyed by call object and result
entry respectively.  Unmatched or ambiguous results remain standalone.
A reused call ID can pair again after its previous result was consumed."
  (let ((pending (make-hash-table :test #'equal))
        (results (make-hash-table :test #'eq))
        (consumed (make-hash-table :test #'eq)))
    (dolist (entry entries)
      (let ((message (plist-get entry :message)))
        (pcase (plist-get message :role)
          ("assistant"
           (seq-doseq (block (plist-get message :content))
             (when (equal (plist-get block :type) "toolCall")
               (let ((id (plist-get block :id)))
                 (when (and (stringp id) (not (string-empty-p id)))
                   (puthash id (if (gethash id pending) :ambiguous block) pending))))))
          ("toolResult"
           (let* ((id (plist-get message :toolCallId))
                  (call (gethash id pending)))
             (when (and (consp call)
                        (or (null (plist-get message :toolName))
                            (equal (plist-get message :toolName) (plist-get call :name))))
               (puthash call entry results)
               (puthash entry t consumed)
               (remhash id pending)))))))
    (cons results consumed)))

(provide 'pilish-org-tools)
;;; pilish-org-tools.el ends here
