;;; pilish-org-view.el --- Read-only Org views of Pi sessions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Optional offline viewer.  Load this file after adding Pilish and a pinned
;; Prisma checkout to load-path, then run M-x pilish-org-view-file.
;; Requires no running Pi process and never rewrites the source JSONL.
;; The view shows historical messages, not the context-edit/compaction-adjusted
;; model context.  Branch choice is local to the viewer, not session navigation.

;;; Code:

(require 'org)
(require 'json)
(require 'pilish-jsonl)
(require 'pilish-org-markdown)
(require 'pilish-org-tools)
(require 'pilish-org-metadata)

(defgroup pilish-org-view nil
  "Read-only Org views of Pi session archives."
  :group 'org)

(defcustom pilish-org-view-show-usage nil
  "Whether to include message usage sections and standalone usage events.
Refresh an existing viewer after changing this option."
  :type 'boolean
  :group 'pilish-org-view)

(defcustom pilish-org-view-show-events nil
  "Whether to render event-kind entries, including bookkeeping boundaries.
Hidden events still participate in branch metadata and thinking-level replay.
Refresh an existing viewer after changing this option."
  :type 'boolean
  :group 'pilish-org-view)

(defcustom pilish-org-view-show-system nil
  "Whether to render system messages.
Refresh an existing viewer after changing this option."
  :type 'boolean
  :group 'pilish-org-view)

(defvar-local pilish--org-view-path nil "Source JSONL pathname.")
(defvar-local pilish--org-view-leaf nil "Selected branch endpoint key.")
(defvar-local pilish--org-view-sparse-configured nil
  "Whether this viewer has initialized its sparse-tree visibility defaults.")

(defvar pilish--org-view-entry-context nil
  "Dynamically bound per-render source-entry metadata table.
Never buffer-local: nested temporary rendering buffers share the context.")

(defun pilish--org-view-one-line (value)
  "Return VALUE as one safe heading, property or minibuffer line."
  (string-trim (replace-regexp-in-string "[\n\r\t ]+" " " (format "%s" (or value "")))))

(defun pilish--org-view-branch (path current &optional key)
  "Describe root-to-endpoint entry PATH, marking CURRENT and using KEY."
  (let* ((last (car (last path)))
         (id (or key (plist-get last :id)))
         (prompt (cl-find-if
                  (lambda (entry)
                    (equal (plist-get (plist-get entry :message) :role) "user"))
                  (reverse path)))
         (preview (pilish--jsonl-extract-text
                   (plist-get (plist-get prompt :message) :content) 80 " ")))
    (list :key id :entries path :current (equal id current)
          :title (format "%s%s  %s  %s"
                         (if (equal id current) "[current] " "")
                         (or id "legacy")
                         (or (plist-get last :timestamp) "")
                         (pilish--org-view-one-line preview)))))

(defun pilish--org-view-branches (session)
  "Return selectable branches for SESSION using Pilish's raw tree.
Include the current position even if it is not a terminal node.  Legacy
linear files with no IDs are displayed as one unaddressable transcript."
  (let* ((entries (plist-get session :entries))
         (current (plist-get session :leafId)))
    (if (and (> (length entries) 0)
             (cl-every (lambda (entry) (null (plist-get entry :id))) entries))
        (list (pilish--org-view-branch (append entries nil) "legacy" "legacy"))
      (let* ((tree (pilish-jsonl-build-tree entries))
             (stack (mapcar (lambda (node) (cons node nil))
                            (append (plist-get tree :tree) nil)))
             branches)
        (while stack
          (let* ((task (pop stack))
                 (node (car task))
                 (entry (plist-get node :entry))
                 (path (cons entry (cdr task)))
                 (children (plist-get node :children)))
            (when (or (= (length children) 0)
                      (equal (plist-get entry :id) current))
              (push (pilish--org-view-branch
                     (reverse path) current
                     (or (plist-get entry :id)
                         (format "legacy-%d" (length branches))))
                    branches))
            (dolist (child (reverse (append children nil)))
              (push (cons child path) stack))))
        (nreverse branches)))))

(defun pilish--org-view-select (branches &optional key)
  "Select from BRANCHES, or resolve endpoint KEY without prompting."
  (cond
   (key (or (cl-find key branches :key (lambda (branch) (plist-get branch :key))
                     :test #'equal)
            (user-error "Branch endpoint no longer exists: %s" key)))
   ((null branches) (user-error "Session has no readable branches"))
   ((null (cdr branches)) (car branches))
   (t
    (let* ((choices (mapcar (lambda (branch)
                             (cons (plist-get branch :title) branch)) branches))
           (default (car (cl-find-if (lambda (pair) (plist-get (cdr pair) :current))
                                    choices))))
      (cdr (assoc (completing-read "Session branch: " choices nil t nil nil default)
                  choices))))))

(defun pilish--org-view-json (value)
  "Format VALUE as readable JSON without changing the source object."
  (let ((json-encoding-pretty-print t)
        (json-null :null)
        (json-false :false))
    (json-encode value)))

(defun pilish--org-view-heading (title entry &optional level folded properties kind)
  "Insert TITLE with tags, classification and source properties from ENTRY.
LEVEL defaults to one.  FOLDED adds a native VISIBILITY property so the
initial fold also survives saving an Org snapshot.  PROPERTIES supplies
additional drawer fields as an alist of string keys and values.  KIND
classifies synthetic sections; source entries otherwise determine it."
  (let* ((kind (or kind (pilish--org-view-kind entry)))
         (context (and entry pilish--org-view-entry-context
                       (gethash entry pilish--org-view-entry-context)))
         (message (plist-get entry :message))
         (role (or (plist-get message :role)
                   (and (null entry) (member kind '("response" "thinking" "tool" "usage" "error"))
                        "assistant")))
         (tool-name (or (cdr (assoc "PI_TOOL_NAME" properties))
                        (pilish--org-view-string (plist-get message :toolName))
                        (and (equal role "bashExecution") "bash")))
         (tags (delete-dups
                (append (pilish--org-view-kind-tags kind tool-name)
                        (plist-get context :tags)
                        (when (equal (cdr (assoc "PI_STATUS" properties)) "error")
                          '("pi_error")))))
         (seen (make-hash-table :test #'equal)))
    (insert (make-string (or level 1) ?*) " " (pilish--org-view-one-line title)
            " :" (string-join tags ":") ":\n:PROPERTIES:\n")
    (when folded (insert ":VISIBILITY: folded\n"))
    (dolist (pair (append
                  (list (cons "PI_KIND" kind)) properties
                  `(("PI_ENTRY_ID" . ,(plist-get entry :id))
                    ("PI_PARENT_ID" . ,(plist-get entry :parentId))
                    ("PI_ENTRY_TYPE" . ,(plist-get entry :type))
                    ("PI_ROLE" . ,role)
                    ("PI_TIMESTAMP" . ,(plist-get entry :timestamp))
                    ("PI_MODEL" . ,(pilish--org-view-string (plist-get message :model)))
                    ("PI_THINKING_LEVEL" . ,(pilish--org-view-string (plist-get message :thinkingLevel)))
                    ("PI_TOOL_NAME" . ,tool-name)
                    ("PI_TOOL_CALL_ID" . ,(plist-get message :toolCallId)))
                  (plist-get context :properties)))
      (when (and (cdr pair) (not (eq (cdr pair) :null)) (not (gethash (car pair) seen)))
        (puthash (car pair) t seen)
        (insert ":" (car pair) ": " (pilish--org-view-one-line (cdr pair)) "\n")))
    (insert ":END:\n")))

(defun pilish--org-view-tool-result (entry &optional call level)
  "Insert result ENTRY, optionally paired with CALL, at heading LEVEL."
  (let* ((message (plist-get entry :message))
         (args (plist-get call :arguments))
         (language (pilish--org-tool-result-language call entry)))
    (pilish--org-view-heading
     (concat (if call "Result" (concat "Tool result: "
                                     (or (plist-get message :toolName) "unknown")))
             (when (eq (plist-get message :isError) t) " [error]"))
     entry level (not call)
     `(("PI_TOOL_NAME" . ,(or (plist-get call :name) (plist-get message :toolName)))
       ("PI_STATUS" . ,(if (eq (plist-get message :isError) t) "error" "ok"))))
    (pilish--org-view-content
     (or (plist-get message :content) (plist-get message :output)) t nil nil
     language (and language (pilish--org-tool-headers args)))))

(defun pilish--org-view-tool (call &optional result entry)
  "Insert CALL and its matching RESULT, with source properties from ENTRY."
  (let* ((name (plist-get call :name))
         (args (plist-get call :arguments))
         (path (pilish--org-tool-path args))
         (body (pilish--org-tool-call-body call result)))
    (pilish--org-view-heading
     (concat "Tool call: " (or name "unknown")
             (when path (concat " — " path))
             (when (eq (plist-get (plist-get result :message) :isError) t) " [error]"))
     entry 2 t
     `(("PI_TOOL_CALL_ID" . ,(plist-get call :id))
       ("PI_TOOL_NAME" . ,name)
       ("PI_TOOL_PATH" . ,path)
       ("PI_STATUS" . ,(cond ((null result) "missing")
                             ((eq (plist-get (plist-get result :message) :isError) t) "error")
                             (t "ok")))) "tool")
    (insert (or body (pilish--org-literal-block
                      (pilish--org-view-json args) "json")) "\n\n")
    (if result (pilish--org-view-tool-result result call 3)
      (insert "[No saved result]\n\n"))))

(defun pilish--org-view-content (content &optional literal base results language headers entry)
  "Insert message CONTENT, using LITERAL output or Markdown beneath BASE.
RESULTS maps tool call objects to results.  LANGUAGE and HEADERS optionally
select source rendering for literal output, otherwise an example block.
ENTRY supplies source properties for assistant thinking and tool sections."
  (cond
   ((stringp content)
    (insert (if literal (pilish--org-literal-block content language headers)
              (pilish-org-markdown-to-org content (or base 1))) "\n\n"))
   ((or (vectorp content) (consp content))
    (seq-doseq (block content)
      (pcase (plist-get block :type)
        ("text" (pilish--org-view-content (or (plist-get block :text) "")
                                          literal base results language headers entry))
        ("image"
         (insert "[Image: " (pilish--org-view-one-line (plist-get block :mimeType))
                 "; binary data retained in source JSONL]\n\n"))
        ("thinking"
         (pilish--org-view-heading "Thinking" entry 2 t nil "thinking")
         ;; Signatures/encrypted payloads are intentionally not rendered.
         (pilish--org-view-content (or (plist-get block :thinking) "[Redacted]") nil 2))
        ("toolCall"
         (pilish--org-view-tool block (and results (gethash block results)) entry))
        (_ (insert (pilish--org-literal-block (pilish--org-view-json block) "json")
                   "\n\n")))))))

(defun pilish--org-view-assistant (message &optional results entry)
  "Insert MESSAGE's blocks and matching RESULTS with source identity ENTRY.
Each section owns its metadata; it must not inherit another message's ID,
model or thinking level from the shared assistant container."
  (let ((start (point)))
    (seq-doseq (block (plist-get message :content))
      (pcase (plist-get block :type)
        ("text"
         (when-let* ((text (pilish--org-view-string (plist-get block :text)))
                     ((string-match-p "\\S-" text)))
           (pilish--org-view-heading "Response" entry 2 nil nil "response")
           (pilish--org-view-content text nil 2)))
        ((or "thinking" "toolCall")
         (pilish--org-view-content (vector block) nil nil results nil nil entry))
        (_
         (pilish--org-view-heading "Content" entry 2 nil nil "assistant")
         (pilish--org-view-content (vector block) nil 2 results))))
    (when-let* ((error-text (plist-get message :errorMessage)))
      (pilish--org-view-heading "Error" entry 2 nil nil "error")
      (insert (pilish--org-literal-block error-text) "\n\n"))
    (when-let* ((pilish-org-view-show-usage)
                (usage (plist-get message :usage)))
      (pilish--org-view-heading "Usage" entry 2 t nil "usage")
      (insert (pilish--org-literal-block (pilish--org-view-json usage) "json") "\n\n"))
    ;; Empty/redacted messages still need an addressable source position.
    (when (= start (point))
      (pilish--org-view-heading "Assistant entry" entry 2 nil nil "assistant"))))

(defun pilish--org-view-entry (entry &optional results continue-assistant)
  "Insert historical ENTRY, grouping tool calls with RESULTS.
CONTINUE-ASSISTANT reuses the preceding top-level assistant container.
Respect the system/event visibility options.  Never apply context edits
to earlier text."
  (when (pcase (pilish--org-view-kind entry)
          ("event" pilish-org-view-show-events)
          ("system" pilish-org-view-show-system)
          (_ t))
    (pcase (plist-get entry :type)
      ("message"
       (let* ((message (plist-get entry :message))
              (role (plist-get message :role)))
         (if (equal role "toolResult")
             (pilish--org-view-tool-result entry)
           (unless (and (equal role "assistant") continue-assistant)
             (pilish--org-view-heading
              (pcase role
                ("user" "User") ("assistant" "Assistant")
                (_ (capitalize (or role "Unknown message"))))
              (unless (equal role "assistant") entry) nil (equal role "system")
              ;; The container's source shortcut visits the first entry, but
              ;; model/graph metadata belongs only to the individual sections.
              (when (equal role "assistant")
                `(("PI_ENTRY_ID" . ,(plist-get entry :id))
                  ("PI_ROLE" . "assistant")))
              (when (equal role "assistant") "assistant")))
           (pcase role
             ("assistant" (pilish--org-view-assistant message results entry))
             ("system"
              (insert (pilish--org-literal-block (pilish--org-view-json message) "json") "\n\n"))
             ("bashExecution"
              (insert (pilish--org-literal-block (or (plist-get message :command) "") "sh")
                      "\n\n" (pilish--org-literal-block (or (plist-get message :output) "")) "\n\n"))
             ((or "compactionSummary" "branchSummary")
              (pilish--org-view-content (plist-get message :summary)))
             (_ (pilish--org-view-content
                 (or (plist-get message :content) (plist-get message :output))))))))
      ((or "compaction" "branch_summary")
       (pilish--org-view-heading
        (if (equal (plist-get entry :type) "compaction") "Compaction" "Branch summary") entry)
       (pilish--org-view-content (plist-get entry :summary)))
      ("custom_message"
       (pilish--org-view-heading
        (concat "Custom message: " (or (plist-get entry :customType) "unknown")) entry)
       (pilish--org-view-content (plist-get entry :content)))
      ;; With events enabled, retain exact bookkeeping branch boundaries
      ;; without dumping extension state.  Hidden usage can also be a split.
      ((or "label" "session_info" "custom"
           (and "usage" (guard (not pilish-org-view-show-usage))))
       (when (and pilish-org-view-show-events pilish--org-view-entry-context
                  (plist-get (gethash entry pilish--org-view-entry-context) :tags))
         (pilish--org-view-heading
          (concat "Branch metadata: " (plist-get entry :type)) entry nil t)
         (insert "Branch boundary at a bookkeeping entry; raw data is available in the source.\n\n")))
      (_
       (pilish--org-view-heading (concat "Event: " (or (plist-get entry :type) "unknown"))
                                entry nil t)
       (insert (pilish--org-literal-block (pilish--org-view-json entry) "json") "\n\n")))))

(defun pilish--org-view-render (session branch)
  "Return Org transcript for SESSION and BRANCH without modifying a buffer."
  (let ((pilish--org-view-entry-context (pilish--org-view-build-metadata session branch)))
    (with-temp-buffer
      (insert "#+title: Pi — "
              (pilish--org-view-one-line
               (or (plist-get session :name)
                   (file-name-base (plist-get session :path)))) "\n"
              "#+startup: showall hidedrawers nohideblocks\n"
              "#+STARTUP: align\n"
              "#+property: header-args :eval never\n\n"
              "Source: " (pilish--org-view-one-line (plist-get session :path)) "\n"
              "Branch endpoint: " (pilish--org-view-one-line (plist-get branch :key)) "\n"
              "Historical transcript; context edits do not rewrite displayed messages.\n\n")
      (when-let* ((diagnostic (plist-get
                              (pilish-jsonl-build-tree (plist-get session :entries))
                              :diagnostic)))
        (insert "Warning: " (pilish--org-view-one-line diagnostic) "\n\n"))
      (let* ((entries (plist-get branch :entries))
             (pairs (pilish--org-tool-pairs entries))
             assistant-open)
        (dolist (entry entries)
          (unless (or (gethash entry (cdr pairs))
                      (and (not pilish-org-view-show-usage)
                           (equal (plist-get entry :type) "usage")
                           (not (plist-get (gethash entry pilish--org-view-entry-context) :tags))))
            (let ((start (point))
                  (assistant (equal (pilish--org-view-kind entry) "assistant")))
              (pilish--org-view-entry entry (car pairs) assistant-open)
              ;; Paired results and omitted entries do not split a run.
              ;; Any visible non-assistant entry does.
              (when (/= start (point))
                (setq assistant-open assistant))))))
      (buffer-string))))

(defun pilish--org-view-no-evaluation (&rest _args)
  "Refuse Babel evaluation in generated transcripts."
  (user-error "Code in a session transcript is not executable"))

(defvar pilish-org-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "g") #'pilish-org-view-refresh)
    (define-key map (kbd "b") #'pilish-org-view-select-branch)
    (define-key map (kbd "s") #'pilish-org-view-source)
    ;; Prefix alternatives also work when Evil owns the single-letter keys.
    (define-key map (kbd "C-c C-r") #'pilish-org-view-refresh)
    (define-key map (kbd "C-c C-b") #'pilish-org-view-select-branch)
    (define-key map (kbd "C-c C-s") #'pilish-org-view-source)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for read-only Org session views.")

(defun pilish--org-view-setup-sparse-trees ()
  "Initialize buffer-local sparse-tree visibility, without changing Org globally."
  ;; pi_prose is never on the assistant envelope: expanding a matching prose
  ;; subtree must not reveal sibling thinking/tool sections.
  (setq-local org-fold-show-context-detail
              (cons '(tags-tree . ancestors-full)
                    (if (listp org-fold-show-context-detail)
                        (assq-delete-all 'tags-tree (copy-tree org-fold-show-context-detail))
                      (list (cons 'default (if org-fold-show-context-detail 'canonical 'minimal))))))
  (setq pilish--org-view-sparse-configured t))

(define-derived-mode pilish-org-view-mode org-mode "Pi-Org"
  "Read-only Org transcript: g refresh, b branch, s source, q quit.
Normal Org folding, navigation, links and export remain available."
  (setq-local buffer-read-only t)
  (setq-local org-export-use-babel nil)
  (setq-local org-confirm-babel-evaluate #'pilish--org-view-no-evaluation)
  (setq-local org-startup-with-inline-images nil)
  (setq-local org-startup-folded 'nofold)
  (setq-local org-cycle-hide-drawer-startup t)
  (setq-local org-cycle-hide-block-startup nil)
  (pilish--org-view-setup-sparse-trees)
  (buffer-disable-undo))

(defun pilish--org-view-load (path &optional key buffer)
  "Render session PATH at endpoint KEY into BUFFER or a new viewer.
Read and convert first so an error leaves an existing view untouched."
  (let* ((session (or (pilish-jsonl-read-file path)
                      (user-error "Not a readable Pi session: %s" path)))
         (branch (pilish--org-view-select (pilish--org-view-branches session) key))
         (text (pilish--org-view-render session branch))
         (buffer (or buffer (generate-new-buffer
                            (format "*Pi Org: %s*" (file-name-base path))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pilish-org-view-mode)
        (let ((org-inhibit-startup t)) (pilish-org-view-mode)))
      ;; Upgrade already-open viewers once after reloading the library, but
      ;; preserve subsequent user overrides to the buffer-local setting.
      (unless pilish--org-view-sparse-configured
        (pilish--org-view-setup-sparse-trees))
      (let ((inhibit-read-only t)
            (old-point (point)))
        (erase-buffer)
        (insert text)
        (setq pilish--org-view-path path
              pilish--org-view-leaf (plist-get branch :key))
        (let ((cwd (plist-get (plist-get session :header) :cwd)))
          (setq default-directory
                (if (and (stringp cwd) (file-name-absolute-p cwd)
                         (not (file-remote-p path)) (not (file-remote-p cwd))
                         (file-directory-p cwd))
                    (file-name-as-directory cwd)
                  (file-name-directory path))))
        ;; Re-read generated STARTUP/PROPERTY keywords, including when an
        ;; already-open viewer predates the current mode defaults.
        (org-set-regexps-and-options)
        ;; Mode startup ran before the rendered text existed.  Apply alignment
        ;; explicitly, and skip literal tool/source/example-block contents.
        (when org-startup-align-all-tables
          (org-table-map-tables #'org-table-align t 'org))
        (org-cycle-set-startup-visibility)
        (goto-char (min old-point (point-max)))
        (set-buffer-modified-p nil)))
    buffer))

;;;###autoload
(defun pilish-org-view-file (path &optional leaf-id)
  "Open Pi session JSONL at PATH as a read-only Org transcript.
Prompt for a branch when several exist, defaulting to the current one.
Optional LEAF-ID selects a branch endpoint without prompting.  Selection
never changes the session's persisted active branch."
  (interactive "fPi session JSONL: ")
  (pop-to-buffer (pilish--org-view-load (expand-file-name path) leaf-id)))

(defun pilish-org-view-refresh ()
  "Reload the selected endpoint from disk; do not follow a different branch."
  (interactive)
  (pilish--org-view-load pilish--org-view-path pilish--org-view-leaf (current-buffer)))

(defun pilish-org-view-select-branch ()
  "Choose a different branch of the viewed session."
  (interactive)
  (pilish--org-view-load pilish--org-view-path nil (current-buffer)))

(defun pilish-org-view-source ()
  "Visit the source JSONL entry at point, or the file header."
  (interactive)
  (let ((id (org-entry-get nil "PI_ENTRY_ID" t))
        (path pilish--org-view-path))
    (find-file-other-window path)
    (goto-char (point-min))
    (when id
      (while (and (not (eobp))
                  (not (equal id (plist-get
                                  (pilish--parse-json-line
                                   (buffer-substring-no-properties
                                    (line-beginning-position) (line-end-position)))
                                  :id))))
        (forward-line 1)))))

(provide 'pilish-org-view)
;;; pilish-org-view.el ends here
