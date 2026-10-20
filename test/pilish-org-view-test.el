;;; pilish-org-view-test.el --- Offline Org viewer tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional tests: require a pinned Prisma checkout and Markdown grammars.

;;; Code:
(require 'pilish-test-common)
(require 'pilish-org-view)
(require 'org-element)

(defun pilish-test--org-grammars ()
  "Skip conversion tests unless both real tree-sitter grammars are available."
  (unless (and (treesit-language-available-p 'markdown)
               (treesit-language-available-p 'markdown-inline))
    (ert-skip "Markdown block and inline grammars are required")))

(defun pilish-test--org-session ()
  "Return a small branched session with its current leaf on branch B."
  (copy-tree '(:path "/tmp/session.jsonl" :header (:type "session" :id "session")
    :leafId "b"
    :entries [(:type "message" :id "u" :parentId :null
               :message (:role "user" :content "Question **one**"))
              (:type "message" :id "a" :parentId "u"
               :message (:role "assistant"
                         :content [(:type "text" :text "Answer A")]))
              (:type "message" :id "b" :parentId "u"
               :message (:role "assistant" :model "test-model"
                         :content [(:type "thinking" :thinking "Reasoning")
                                   (:type "text" :text "# Answer B\n\n**bold**")
                                   (:type "toolCall" :id "call-1" :name "bash"
                                    :arguments (:command "echo hello"))]))]) t))

(ert-deftest pilish-test-org-markdown-structure ()
  (pilish-test--org-grammars)
  (let ((org (pilish-org-markdown-to-org
              "# Heading\n\nTitle\n=====\n\n**bold** and *italic* and `code` [label](https://example.org).\n\n- [x] parent\n  - child\n\n  second paragraph\n\n1) numbered\n\n> quoted\n\n| A | B |\n|---|---|\n| one | two |\n" 2)))
    (dolist (part '("*** Heading" "*** Title" "*bold*" "/italic/" "~code~"
                    "[[https://example.org][label]]" "[X] parent" "  - child"
                    "second paragraph" "1. numbered" "#+begin_quote" "quoted"
                    "| A | B |"))
      (should (string-search part org)))))

(ert-deftest pilish-test-org-markdown-preserves-unknown-blocks ()
  (pilish-test--org-grammars)
  (let ((org (pilish-org-markdown-to-org
              "<div>important HTML</div>\n\n[ref]: https://example.org\n\n    indented code\n\n![alt](image.png)\n")))
    (dolist (part '("important HTML" "https://example.org" "indented code" "[[image.png]]"))
      (should (string-search part org)))))

(ert-deftest pilish-test-org-markdown-unicode-and-code-boundaries ()
  (pilish-test--org-grammars)
  (let ((text (pilish-org-markdown-to-org
               "Привет **мир**\n\n```elisp\n#+end_src\n* fake heading\n,#+end_src\n```\n" 1)))
    (should (string-search "Привет *мир*" text))
    (should (string-search ",#+end_src" text))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (let ((ast (org-element-parse-buffer)))
        (should (= 1 (length (org-element-map ast 'src-block #'identity))))
        (should-not (org-element-map ast 'headline #'identity))))))

(ert-deftest pilish-test-org-markdown-incomplete-code ()
  (pilish-test--org-grammars)
  (let ((text (pilish-org-markdown-to-org "```python\nprint('partial')\n")))
    (should (string-search "print('partial')" text))
    (should (string-search "#+end_src" text))))

(ert-deftest pilish-test-org-markdown-table-pipes ()
  (pilish-test--org-grammars)
  (let ((text (pilish-org-markdown-to-org "| A |\n|---|\n| a\\|b |\n")))
    (should (string-search "a\\vert{}b" text))))

(ert-deftest pilish-test-org-branches-isolate-siblings ()
  (let* ((branches (pilish--org-view-branches (pilish-test--org-session)))
         (a (pilish--org-view-select branches "a"))
         (b (pilish--org-view-select branches "b")))
    (should (= 2 (length branches)))
    (should (equal '("u" "a") (mapcar (lambda (e) (plist-get e :id))
                                       (plist-get a :entries))))
    (should (equal '("u" "b") (mapcar (lambda (e) (plist-get e :id))
                                       (plist-get b :entries))))
    (should (plist-get b :current))
    (should-error (pilish--org-view-select branches "absent") :type 'user-error)))

(ert-deftest pilish-test-org-branches-current-nonterminal-and-legacy ()
  (let* ((session (pilish-test--org-session))
         (_ (setq session (plist-put session :leafId "u")))
         (branches (pilish--org-view-branches session)))
    (should (= 3 (length branches)))
    (should (plist-get (pilish--org-view-select branches "u") :current)))
  (let* ((session '(:entries [(:type "message" :message (:role "user" :content "a"))
                              (:type "message" :message (:role "assistant" :content []))]))
         (branches (pilish--org-view-branches session)))
    (should (= 1 (length branches)))
    (should (= 2 (length (plist-get (car branches) :entries))))))

(ert-deftest pilish-test-org-branch-completion-default ()
  (let ((branches (pilish--org-view-branches (pilish-test--org-session))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _collection _predicate require-match _initial _history default &rest _)
                 (should require-match)
                 (should (string-prefix-p "[current] b" default))
                 default)))
      (should (equal "b" (plist-get (pilish--org-view-select branches) :key))))))

(ert-deftest pilish-test-org-view-render-messages-and-tool-results ()
  (pilish-test--org-grammars)
  (let* ((session (pilish-test--org-session))
         (branch (pilish--org-view-select (pilish--org-view-branches session) "b"))
         (text (pilish--org-view-render session branch)))
    (should (string-search "Question *one*" text))
    (should (string-search "*** Answer B" text))
    (should-not (string-search "Answer A" text))
    (should (string-search ":PI_ENTRY_ID: b" text))
    (should (string-search ":PI_MODEL: test-model" text))
    (should (string-search "** Thinking :pi_thinking:pi_branch_start:\n:PROPERTIES:\n:VISIBILITY: folded" text))
    (should (string-search "Reasoning" text))
    (should (string-search "** Tool call: bash" text)))
  (with-temp-buffer
    (pilish--org-view-entry
     '(:type "message" :id "result"
       :message (:role "toolResult" :toolCallId "call-1" :toolName "bash" :isError t
                 :content [(:type "text" :text "* literal output\n#+end_example")
                           (:type "image" :mimeType "image/png" :data "SECRET")])) )
    (should (string-search "Tool result: bash [error]" (buffer-string)))
    (should (string-search ",* literal output" (buffer-string)))
    (should-not (string-search "SECRET" (buffer-string)))))

(ert-deftest pilish-test-org-view-context-edits-are-events ()
  (pilish-test--org-grammars)
  (let ((pilish-org-view-show-events t))
    (with-temp-buffer
      (pilish--org-view-entry
       '(:type "message" :id "u" :message (:role "user" :content "original")))
      (pilish--org-view-entry
       '(:type "context_edit" :id "e" :targetId "u" :replacement "replacement"))
      (should (string-search "original" (buffer-string)))
      (should (string-search "* Event: context_edit" (buffer-string))))))

(ert-deftest pilish-test-org-view-readonly-refresh-and-source-unchanged ()
  (pilish-test--org-grammars)
  (let ((file (make-temp-file "pilish-org-" nil ".jsonl")) buffer original)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (json-encode '(:type "session" :version 3 :id "session")) "\n")
            (seq-doseq (entry (plist-get (pilish-test--org-session) :entries))
              (insert (let ((json-null :null)) (json-encode entry)) "\n")))
          (setq original (with-temp-buffer (insert-file-contents-literally file) (buffer-string))
                buffer (pilish--org-view-load file "b"))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-mode))
            (should buffer-read-only)
            (should-not buffer-file-name)
            (should-not (buffer-modified-p))
            (should-not org-export-use-babel)
            (should-error (insert "mutation") :type 'buffer-read-only)
            ;; Existing viewers also pick up the generated per-file folding
            ;; settings on refresh, even if their previous defaults differed.
            (setq-local org-startup-folded 'showeverything)
            (setq-local org-cycle-hide-drawer-startup nil)
            (pilish-org-view-refresh)
            (should (equal pilish--org-view-leaf "b"))
            (should (eq org-startup-folded 'showall))
            (should org-cycle-hide-drawer-startup)
            (goto-char (point-min))
            (search-forward ":PI_ENTRY_ID: u")
            (should (org-invisible-p))
            (should (equal original (with-temp-buffer
                                      (insert-file-contents-literally file)
                                      (buffer-string))))
            (font-lock-ensure)
            (let ((before (buffer-string)))
              (with-temp-file file (insert "invalid session\n"))
              (should-error (pilish-org-view-refresh) :type 'user-error)
              (should (equal before (buffer-string)))))
          ;; Restore the fixture, then verify rendering never wrote the archive.
          (with-temp-file file (insert original))
          (with-current-buffer buffer (pilish-org-view-refresh))
          (should (equal original (with-temp-buffer
                                    (insert-file-contents-literally file)
                                    (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest pilish-test-org-view-golden-branches ()
  (pilish-test--org-grammars)
  (dolist (fixture '("browse-session.jsonl" "browse-session-v087.jsonl"))
    (let* ((session (pilish-jsonl-read-file (expand-file-name fixture pilish-test--fixture-dir)))
           (branches (pilish--org-view-branches session)))
      (should branches)
      (dolist (branch branches)
        (let ((text (pilish--org-view-render session branch)))
          (should (string-prefix-p "#+title: Pi" text))
          (should (> (length text) 100)))))))

(ert-deftest pilish-test-org-view-json-booleans-and-null ()
  (should (equal '(:parent :null :error :false)
                 (json-parse-string
                  (pilish--org-view-json '(:parent :null :error :false))
                  :object-type 'plist))))

(ert-deftest pilish-test-org-view-export-does-not-evaluate ()
  (require 'ox-html)
  (with-temp-buffer
    (pilish-org-view-mode)
    (let ((inhibit-read-only t))
      (insert (pilish--org-literal-block "(error \"must not run\")" "emacs-lisp")))
    (cl-letf (((symbol-function 'org-babel-execute-src-block)
               (lambda (&rest _) (ert-fail "Export attempted Babel execution"))))
      (let ((org-html-htmlize-output-type nil))
        (should (string-search "must not run" (org-export-as 'html nil nil t)))))
    (should-error (pilish--org-view-no-evaluation) :type 'user-error)))

(ert-deftest pilish-test-org-view-source-lookup-inherits-message-id ()
  (let ((file (make-temp-file "pilish-org-source-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"type\":\"session\",\"id\":\"session\"}\n"
                    "{\"type\":\"message\",\"id\":\"target\"}\n"))
          (with-temp-buffer
            (insert "* Assistant\n:PROPERTIES:\n:PI_ENTRY_ID: target\n:END:\n** Response\nbody\n")
            (pilish-org-view-mode)
            (setq pilish--org-view-path file)
            (goto-char (point-max))
            (cl-letf (((symbol-function 'find-file-other-window)
                       (lambda (path) (set-buffer (find-file-noselect path)))))
              (save-current-buffer
                (pilish-org-view-source)
                (should (= 2 (line-number-at-pos)))))))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(defun pilish-test--org-tool-transcript (name args output &optional error)
  "Render a selected path with tool NAME, ARGS and OUTPUT, optionally ERROR."
  (let* ((entries (list
                   (list :type "message" :id "a"
                         :message (list :role "assistant"
                                        :content (vector (list :type "toolCall" :id "call"
                                                               :name name :arguments args))))
                   (list :type "message" :id "r" :parentId "a"
                         :message (list :role "toolResult" :toolCallId "call"
                                        :toolName name :isError error
                                        :content (vector (list :type "text" :text output))))))
         (session (list :path "/tmp/tool.jsonl" :entries (vconcat entries))))
    (pilish--org-view-render session (list :key "r" :entries entries))))

(ert-deftest pilish-test-org-tool-bash-grouped-with-result ()
  (let ((text (pilish-test--org-tool-transcript
               "bash" '(:command "echo hello" :timeout 10) "hello")))
    (should (string-search "#+begin_src bash :timeout 10 :eval never\necho hello" text))
    (should (string-search "*** Result :pi_tool_result:" text))
    (should (string-search ":PI_ENTRY_ID: r" text))
    (should (string-search "#+begin_example\nhello\n#+end_example" text))
    (should-not (string-search "* Tool result:" text)))
  (should (string-search "bash :timeout nil :eval never"
                         (pilish-test--org-tool-transcript
                          "bash" '(:command "false" :timeout :null) "failed" t))))

(ert-deftest pilish-test-org-tool-read-language-and-fallback ()
  (let ((text (pilish-test--org-tool-transcript
               "read" '(:path "src/app.py" :offset 5 :limit 10) "print('hello')")))
    (should (string-search "Tool call: read — src/app.py" text))
    (should (string-search "#+begin_src python :path \"src/app.py\" :offset 5 :limit 10 :eval never" text)))
  (dolist (case '(("mystery.unknown-extension" nil)
                  ("src/app.py" t)))
    (let ((text (pilish-test--org-tool-transcript
                 "read" (list :path (car case)) "not source code" (cadr case))))
      (should (string-search "#+begin_example\nnot source code" text))
      (should-not (string-search "#+begin_src python" text))))
  (cl-letf (((symbol-function 'find-file-noselect)
             (lambda (&rest _) (ert-fail "Language detection opened a file"))))
    (let ((auto-mode-alist '(("\\.abc\\'" . python-mode))))
      (should (equal "python" (pilish--org-tool-language "missing.abc")))))
  (should (equal "markdown" (pilish--org-tool-language "README.md")))
  (should (equal "makefile" (pilish--org-tool-language "Makefile"))))

(ert-deftest pilish-test-org-tool-edit-diff-and-legacy-args ()
  (let ((text (pilish-test--org-tool-transcript
               "edit" '(:path "x.py" :edits [(:oldText "old\n" :newText "new\n")
                                              (:oldText "" :newText "inserted")]) "replaced")))
    (should (string-search "snippet-relative" text))
    (should (string-search "#+begin_src diff :eval never" text))
    (should (string-search "@@ -1,1 +1,1 @@\n-old\n+new" text))
    (should (string-search "@@ -0,0 +1,1 @@\n+inserted\n\\ No newline at end of file" text))
    (should (string-search "*** Result" text)))
  (should (equal 1 (length (pilish--org-tool-edit-diffs
                           '(:path "x.el" :oldText "old" :newText "new")))))
  (should-not (pilish--org-tool-edit-diffs '(:edits [(:unknown "shape")])))
  (let ((text (pilish-test--org-tool-transcript "edit" '(:unknown "shape") "failed" t)))
    (should (string-search "#+begin_src json" text))
    (should (string-search "[error]" text))))

(ert-deftest pilish-test-org-tool-pairing-reused-id-and-orphans ()
  (let* ((call1 '(:type "toolCall" :id "same" :name "bash"))
         (call2 (copy-tree call1))
         (assistant1 (list :message (list :role "assistant" :content (vector call1))))
         (assistant2 (list :message (list :role "assistant" :content (vector call2))))
         (orphan '(:message (:role "toolResult" :toolCallId "missing")))
         (result1 '(:id "r1" :message (:role "toolResult" :toolCallId "same")))
         (result2 '(:id "r2" :message (:role "toolResult" :toolCallId "same")))
         (pairs (pilish--org-tool-pairs (list orphan assistant1 result1 assistant2 result2))))
    (should (eq result1 (gethash call1 (car pairs))))
    (should (eq result2 (gethash call2 (car pairs))))
    (should-not (gethash orphan (cdr pairs)))
    ;; A different selected path must not borrow another path's result.
    (should-not (gethash call1 (car (pilish--org-tool-pairs (list assistant1 orphan))))))
  (let* ((call '(:type "toolCall" :id "same" :name "bash"))
         (entries (list (list :message (list :role "assistant"
                                             :content (vector call (copy-tree call))))
                        '(:message (:role "toolResult" :toolCallId "same"))))
         (pairs (pilish--org-tool-pairs entries)))
    (should (= 0 (hash-table-count (car pairs))))
    (should (= 0 (hash-table-count (cdr pairs))))))

(ert-deftest pilish-test-org-view-usage-default-and-opt-in ()
  (let* ((entries '((:type "message" :id "a"
                     :message (:role "assistant" :content [] :usage (:input 10)))
                    (:type "usage" :id "u" :parentId "a" :kind "cache_warm")))
         (session (list :path "/tmp/usage.jsonl" :entries (vconcat entries)))
         (branch (list :key "u" :entries entries)))
    (let ((pilish-org-view-show-usage nil))
      (let ((text (pilish--org-view-render session branch)))
        (should-not (string-search "** Usage" text))
        (should-not (string-search "Event: usage" text))))
    (let ((pilish-org-view-show-usage t))
      (let ((text (pilish--org-view-render session branch)))
        (should (string-search "** Usage" text))
        (should (string-search "Event: usage" text))))))

(ert-deftest pilish-test-org-tool-headers-and-escaping ()
  (let* ((command "echo hi\n#+end_src\n* not a heading")
         (headers (pilish--org-tool-headers
                   '(:timeout :null :cwd "a\nb :eval yes" :eval "yes"
                     :var "x=(error \"must not run\")")))
         (text (pilish--org-literal-block command "bash" headers)))
    (should-not (string-search "\n" headers))
    (should (string-search ":pi-eval \"yes\"" headers))
    (should (string-search ":pi-var" headers))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (let ((blocks (org-element-map (org-element-parse-buffer) 'src-block #'identity)))
        (should (= 1 (length blocks)))
        (let ((params (org-babel-parse-header-arguments
                       (org-element-property :parameters (car blocks)) t)))
          (should (equal "never" (cdr (assq :eval params)))))))))

(ert-deftest pilish-test-org-view-drawers-fold-in-view-and-snapshot ()
  (let ((text (pilish-test--org-tool-transcript "bash" '(:command "echo hi") "hi")))
    (dolist (snapshot '(nil t))
      (with-temp-buffer
        (insert text)
        ;; Deliberately hostile global defaults: the generated file's
        ;; STARTUP keywords should still hide its drawers on a fresh visit.
        (let ((org-startup-folded 'showeverything)
              (org-cycle-hide-drawer-startup nil)
              (org-inhibit-startup nil))
          (if snapshot (org-mode)
            (pilish-org-view-mode)
            (org-cycle-set-startup-visibility)))
        (goto-char (point-min))
        (search-forward ":PI_ENTRY_ID: a")
        (should (org-invisible-p))
        (goto-char (point-min))
        (search-forward "\necho hi")
        (should (org-invisible-p))
        (goto-char (point-min))
        (search-forward "* Assistant")
        (should-not (org-invisible-p))))))

(ert-deftest pilish-test-org-tool-edit-prefers-recorded-successful-diff ()
  (dolist (error '(nil t))
    (with-temp-buffer
      (pilish--org-view-tool
       '(:type "toolCall" :id "edit" :name "edit"
         :arguments (:path "x.py" :oldText "old" :newText "new"))
       (list :type "message" :id "result"
             :message (list :role "toolResult" :toolCallId "edit" :isError error
                            :details '(:diff "+42 actual recorded change")
                            :content [(:type "text" :text "status")])))
      (if error
          (progn
            (should (string-search "Requested replacements" (buffer-string)))
            (should-not (string-search "+42 actual" (buffer-string))))
        (should (string-search "+42 actual recorded change" (buffer-string)))
        (should-not (string-search "Requested replacements" (buffer-string)))))))

(ert-deftest pilish-test-org-tool-pairing-out-of-order-results ()
  (let* ((call1 '(:type "toolCall" :id "one" :name "bash"))
         (call2 '(:type "toolCall" :id "two" :name "read"))
         (result1 '(:message (:role "toolResult" :toolCallId "one" :toolName "bash")))
         (result2 '(:message (:role "toolResult" :toolCallId "two" :toolName "read")))
         (pairs (pilish--org-tool-pairs
                 (list (list :message (list :role "assistant" :content (vector call1 call2)))
                       result2 result1))))
    (should (eq result1 (gethash call1 (car pairs))))
    (should (eq result2 (gethash call2 (car pairs))))
    (should (= 2 (hash-table-count (cdr pairs))))))

(ert-deftest pilish-test-org-view-branch-metadata-counts-off-path-children ()
  (let* ((session (pilish-test--org-session))
         (before (copy-tree session t))
         (branches (pilish--org-view-branches session))
         (branch (pilish--org-view-select branches "b"))
         (metadata (pilish--org-view-build-metadata session branch))
         (user (aref (plist-get session :entries) 0))
         (assistant (aref (plist-get session :entries) 2)))
    (should (equal 2 (cdr (assoc "PI_CHILD_COUNT" (plist-get (gethash user metadata) :properties)))))
    (should (member "pi_branchpoint" (plist-get (gethash user metadata) :tags)))
    (should (equal "b" (cdr (assoc "PI_SELECTED_CHILD_ID"
                                  (plist-get (gethash user metadata) :properties)))))
    (should (equal 0 (cdr (assoc "PI_CHILD_COUNT" (plist-get (gethash assistant metadata) :properties)))))
    (should (member "pi_branch_start" (plist-get (gethash assistant metadata) :tags)))
    (should (equal "u" (cdr (assoc "PI_BRANCH_FROM_ID"
                                  (plist-get (gethash assistant metadata) :properties)))))
    (should (equal before session))))

(ert-deftest pilish-test-org-view-thinking-level-replays-selected-path-only ()
  (let* ((entries '((:type "thinking_level_change" :id "t0" :thinkingLevel "off")
                    (:type "message" :id "u" :parentId "t0"
                     :message (:role "user" :content "question"))
                    (:type "message" :id "a" :parentId "u"
                     :message (:role "assistant" :model "test" :content []))
                    (:type "thinking_level_change" :id "t1" :parentId "a" :thinkingLevel "high")
                    (:type "message" :id "b" :parentId "t1"
                     :message (:role "assistant" :thinkingLevel "low" :content []))
                    (:type "message" :id "c" :parentId "b"
                     :message (:role "assistant" :thinkingLevel :null :content []))
                    (:type "thinking_level_change" :id "other" :parentId "u" :thinkingLevel "xhigh")))
         (session (list :path "/tmp/thinking.jsonl" :entries (vconcat entries)))
         (branch (list :key "c" :entries (butlast entries))))
    (with-temp-buffer
      (insert (pilish--org-view-render session branch))
      (org-mode)
      (dolist (case '(("a" . "off") ("b" . "low") ("c" . "high")))
        (goto-char (point-min))
        ;; The shared container has only a source shortcut, not a model or
        ;; thinking level that could leak into later messages.
        (search-forward (concat ":PI_ENTRY_ID: " (car case) "\n"))
        (when (= (org-outline-level) 1)
          (search-forward (concat ":PI_ENTRY_ID: " (car case) "\n")))
        (should (equal (cdr case) (org-entry-get nil "PI_THINKING_LEVEL")))))
    (let* ((unknown (list :key "a" :entries (list (nth 2 entries))))
           (text (pilish--org-view-render session unknown)))
      (should-not (string-search ":PI_THINKING_LEVEL:" text)))))

(ert-deftest pilish-test-org-view-hidden-bookkeeping-branchpoint-is-addressable ()
  (let* ((pilish-org-view-show-events t)
         (entries '((:type "message" :id "u" :message (:role "user" :content "question"))
                    (:type "custom" :id "split" :parentId "u" :data (:secret "not displayed"))
                    (:type "message" :id "a" :parentId "split"
                     :message (:role "assistant" :content []))
                    (:type "message" :id "b" :parentId "split"
                     :message (:role "assistant" :content []))))
         (session (list :path "/tmp/branch.jsonl" :entries (vconcat entries)))
         (text (pilish--org-view-render session (list :key "b" :entries
                                                     (list (nth 0 entries) (nth 1 entries) (nth 3 entries))))))
    (should (string-search "Branch metadata: custom :pi_event:pi_branchpoint:" text))
    (should (string-search ":PI_ENTRY_ID: split" text))
    (should (string-search ":PI_CHILD_COUNT: 2" text))
    (should-not (string-search "not displayed" text))))

(defun pilish-test--org-filter-transcript ()
  "Return a transcript containing prose, thinking, and two different tools."
  (let* ((entries '((:type "message" :id "u"
                     :message (:role "user" :content "# User heading\n\nUSER_BODY"))
                    (:type "message" :id "a" :parentId "u"
                     :message (:role "assistant" :thinkingLevel "high" :model "test-model"
                               :content [(:type "thinking" :thinking "THINKING_BODY")
                                         (:type "text" :text "# Answer heading\n\nPROSE_BODY")
                                         (:type "toolCall" :id "c1" :name "bash"
                                          :arguments (:command "BASH_COMMAND"))
                                         (:type "toolCall" :id "c2" :name "read"
                                          :arguments (:path "x.py"))]))
                    (:type "message" :id "r1" :parentId "a"
                     :message (:role "toolResult" :toolName "bash" :toolCallId "c1"
                               :content [(:type "text" :text "BASH_OUTPUT")]))
                    (:type "message" :id "r2" :parentId "r1"
                     :message (:role "toolResult" :toolName "read" :toolCallId "c2"
                               :content [(:type "text" :text "READ_OUTPUT")]))) )
         (session (list :path "/tmp/filter.jsonl" :entries (vconcat entries))))
    (pilish--org-view-render session (list :key "r2" :entries entries))))

(defun pilish-test--org-visible (text)
  "Return whether TEXT is visible anywhere in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (visible)
      (while (search-forward text nil t)
        (when (not (org-invisible-p (match-beginning 0))) (setq visible t)))
      visible)))

(ert-deftest pilish-test-org-view-native-prose-sparse-tree ()
  (pilish-test--org-grammars)
  (dolist (query '("pi_prose" "PI_KIND=\"user\"|PI_KIND=\"response\""))
    (with-temp-buffer
      (insert (pilish-test--org-filter-transcript))
      (pilish-org-view-mode)
      (let ((org-use-tag-inheritance t)
            (org-tags-match-list-sublevels t))
        (org-match-sparse-tree nil query))
      (should (pilish-test--org-visible "USER_BODY"))
      (should (pilish-test--org-visible "PROSE_BODY"))
      (dolist (hidden '("THINKING_BODY" "BASH_COMMAND" "BASH_OUTPUT" "READ_OUTPUT"))
        (should-not (pilish-test--org-visible hidden))))))

(ert-deftest pilish-test-org-view-native-tool-sparse-tree-and-narrowing ()
  (pilish-test--org-grammars)
  (dolist (query '("pi_tool_bash" "PI_TOOL_NAME=\"bash\"" "PI_TOOL_CALL_ID=\"c1\""))
    (with-temp-buffer
      (insert (pilish-test--org-filter-transcript))
      (pilish-org-view-mode)
      (org-match-sparse-tree nil query)
      (should (pilish-test--org-visible "BASH_COMMAND"))
      (should (pilish-test--org-visible "BASH_OUTPUT"))
      (dolist (hidden '("USER_BODY" "PROSE_BODY" "THINKING_BODY" "READ_OUTPUT"))
        (should-not (pilish-test--org-visible hidden)))))
  (with-temp-buffer
    (insert (pilish-test--org-filter-transcript))
    (org-mode)
    (goto-char (point-min))
    (search-forward "** Tool call: bash")
    (org-narrow-to-subtree)
    (should (string-search "BASH_OUTPUT" (buffer-string)))
    (should-not (string-search "READ_OUTPUT" (buffer-string)))
    (widen)
    (should (string-search "READ_OUTPUT" (buffer-string)))))

(ert-deftest pilish-test-org-view-native-branch-property-sparse-tree ()
  (pilish-test--org-grammars)
  (let* ((session (pilish-test--org-session))
         (branch (pilish--org-view-select (pilish--org-view-branches session) "b")))
    (with-temp-buffer
      (insert (pilish--org-view-render session branch))
      (pilish-org-view-mode)
      (org-match-sparse-tree nil "PI_CHILD_COUNT>1")
      (should (pilish-test--org-visible "Question *one*"))
      (should-not (pilish-test--org-visible "Reasoning")))))

(ert-deftest pilish-test-org-view-dispatch-only-assistant-is-not-prose ()
  (with-temp-buffer
    (pilish--org-view-entry
     '(:type "message" :id "dispatch"
       :message (:role "assistant" :content [(:type "text" :text " \n")
                                              (:type "toolCall" :id "c" :name "mcp.example/search"
                                               :arguments (:query "example"))])))
    (should-not (string-search ":pi_prose:" (buffer-string)))
    (should-not (string-search "** Response" (buffer-string)))
    (should (string-search ":pi_tool_mcp_example_search:" (buffer-string)))
    (should (string-search ":PI_TOOL_NAME: mcp.example/search" (buffer-string)))))

(ert-deftest pilish-test-org-view-sparse-configuration-is-buffer-local ()
  (let ((before (copy-tree (default-value 'org-fold-show-context-detail))))
    (with-temp-buffer
      (pilish-org-view-mode)
      (should (eq 'ancestors-full (cdr (assq 'tags-tree org-fold-show-context-detail)))))
    (should (equal before (default-value 'org-fold-show-context-detail)))))

(defun pilish-test--org-grouped-session ()
  "Return assistant runs with paired results, hidden entries and boundaries."
  (let ((entries
         (copy-tree
          '((:type "message" :id "a"
             :message (:role "assistant" :model "first-model" :thinkingLevel "high"
                       :content [(:type "thinking" :thinking "FIRST_THINKING")
                                 (:type "text" :text "# First heading\n\nFIRST_BODY")
                                 (:type "toolCall" :id "c1" :name "bash"
                                  :arguments (:command "FIRST_COMMAND"))]))
            (:type "message" :id "r1"
             :message (:role "toolResult" :toolCallId "c1" :toolName "bash"
                       :content [(:type "text" :text "FIRST_RESULT")]))
            (:type "usage" :id "usage" :kind "cache_warm")
            (:type "label" :id "label" :label "hidden")
            (:type "message" :id "b"
             :message (:role "assistant" :model "second-model" :thinkingLevel "low"
                       :errorMessage "SECOND_ERROR"
                       :content [(:type "thinking" :thinking "SECOND_THINKING")
                                 (:type "text" :text "# Second heading\n\nSECOND_BODY")
                                 (:type "toolCall" :id "c2" :name "bash"
                                  :arguments (:command "SECOND_COMMAND"))
                                 (:type "image" :mimeType "image/png" :data "SECRET")]))
            (:type "message" :id "r2"
             :message (:role "toolResult" :toolCallId "c2" :toolName "bash"
                       :content [(:type "text" :text "SECOND_RESULT")]))
            (:type "message" :id "empty" :message (:role "assistant" :content []))
            (:type "message" :id "sys" :message (:role "system" :content "system"))
            (:type "message" :id "d"
             :message (:role "assistant" :content [(:type "text" :text "AFTER_SYSTEM")]))
            (:type "message" :id "u" :message (:role "user" :content "USER_BODY"))
            (:type "message" :id "e"
             :message (:role "assistant" :content [(:type "text" :text "AFTER_USER")]))
            (:type "message" :id "orphan"
             :message (:role "toolResult" :toolCallId "missing" :toolName "bash"
                       :content [(:type "text" :text "ORPHAN_RESULT")]))
            (:type "message" :id "f"
             :message (:role "assistant" :content [(:type "text" :text "AFTER_ORPHAN")]))) t))
        parent)
    (setq entries
          (mapcar (lambda (entry)
                    (prog1 (plist-put entry :parentId (or parent :null))
                      (setq parent (plist-get entry :id))))
                  entries))
    (list :path "/tmp/grouped.jsonl" :leafId "f" :entries (vconcat entries))))

(ert-deftest pilish-test-org-view-groups-consecutive-assistants ()
  (pilish-test--org-grammars)
  (let* ((session (pilish-test--org-grouped-session))
         (before (copy-tree session t))
         (branch (pilish--org-view-select (pilish--org-view-branches session) "f"))
         (pilish-org-view-show-system t)
         (pilish-org-view-show-usage nil))
    (with-temp-buffer
      (insert (pilish--org-view-render session branch))
      (org-mode)
      (let (kinds)
        (org-map-entries
         (lambda ()
           (when (= (org-outline-level) 1)
             (push (org-entry-get nil "PI_KIND") kinds))))
        (should (equal (nreverse kinds)
                       '("assistant" "system" "assistant" "user"
                         "assistant" "tool_result" "assistant"))))
      (dolist (case '(("FIRST_THINKING" "a" "first-model" "high")
                      ("FIRST_BODY" "a" "first-model" "high")
                      ("FIRST_COMMAND" "a" "first-model" "high")
                      ("SECOND_THINKING" "b" "second-model" "low")
                      ("SECOND_BODY" "b" "second-model" "low")
                      ("SECOND_COMMAND" "b" "second-model" "low")
                      ("SECOND_ERROR" "b" "second-model" "low")
                      ("[Image:" "b" "second-model" "low")))
        (goto-char (point-min))
        (search-forward (car case))
        (should (equal (nth 1 case) (org-entry-get nil "PI_ENTRY_ID" t)))
        (should (equal (nth 2 case) (org-entry-get nil "PI_MODEL" t)))
        (should (equal (nth 3 case) (org-entry-get nil "PI_THINKING_LEVEL" t))))
      (goto-char (point-min))
      (search-forward ":PI_ENTRY_ID: empty\n")
      (should (= 2 (org-outline-level)))
      (should-not (org-entry-get nil "PI_MODEL" t))
      (should-not (org-entry-get nil "PI_THINKING_LEVEL" t)))
    (should (equal before session))))

(ert-deftest pilish-test-org-view-grouped-source-jumps ()
  (pilish-test--org-grammars)
  (let ((file (make-temp-file "pilish-org-grouped-" nil ".jsonl")) buffer)
    (unwind-protect
        (let ((pilish-org-view-show-usage nil))
          (with-temp-file file
            (insert (json-encode '(:type "session" :version 3 :id "session")) "\n")
            (seq-doseq (entry (plist-get (pilish-test--org-grouped-session) :entries))
              (insert (let ((json-null :null)) (json-encode entry)) "\n")))
          (setq buffer (pilish--org-view-load file "f"))
          (dolist (case '(("* Assistant :" . "a")
                          ("FIRST_THINKING" . "a") ("First heading" . "a")
                          ("FIRST_BODY" . "a") ("FIRST_COMMAND" . "a")
                          ("FIRST_RESULT" . "r1") ("SECOND_THINKING" . "b")
                          ("Second heading" . "b") ("SECOND_BODY" . "b")
                          ("SECOND_COMMAND" . "b") ("SECOND_RESULT" . "r2")
                          ("[Image:" . "b") ("SECOND_ERROR" . "b")
                          (":PI_ENTRY_ID: empty" . "empty")
                          ("AFTER_SYSTEM" . "d") ("USER_BODY" . "u")
                          ("AFTER_USER" . "e") ("ORPHAN_RESULT" . "orphan")
                          ("AFTER_ORPHAN" . "f")))
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward (car case))
              (cl-letf (((symbol-function 'find-file-other-window)
                         (lambda (path) (set-buffer (find-file-noselect path)))))
                (save-current-buffer
                  (pilish-org-view-source)
                  (should (equal (cdr case)
                                 (plist-get (pilish--parse-json-line
                                             (buffer-substring-no-properties
                                              (line-beginning-position) (line-end-position)))
                                            :id))))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when-let* ((source (get-file-buffer file))) (kill-buffer source))
      (delete-file file))))

(ert-deftest pilish-test-org-view-grouped-native-sparse-trees ()
  (pilish-test--org-grammars)
  (let* ((session (pilish-test--org-grouped-session))
         (entries (plist-get session :entries))
         ;; Add an off-path alternative at the first assistant entry.
         (_ (setf (plist-get session :entries)
                  (vconcat entries '((:type "message" :id "alt" :parentId "a"
                                      :message (:role "assistant" :content []))))))
         (branch (pilish--org-view-select (pilish--org-view-branches session) "f"))
         (pilish-org-view-show-usage nil)
         (text (pilish--org-view-render session branch)))
    (dolist (query '("pi_prose+PI_MODEL=\"second-model\"+PI_THINKING_LEVEL=\"low\""
                     "PI_CHILD_COUNT>1"))
      (with-temp-buffer
        (insert text)
        (pilish-org-view-mode)
        (org-match-sparse-tree nil query)
        (if (equal query "PI_CHILD_COUNT>1")
            (progn
              (should (pilish-test--org-visible "FIRST_BODY"))
              (should-not (pilish-test--org-visible "SECOND_BODY")))
          (should (pilish-test--org-visible "SECOND_BODY"))
          (dolist (hidden '("FIRST_BODY" "SECOND_THINKING" "SECOND_COMMAND" "SECOND_RESULT"))
            (should-not (pilish-test--org-visible hidden))))))))

(ert-deftest pilish-test-org-view-visible-bookkeeping-splits-assistant-run ()
  (let* ((pilish-org-view-show-events t)
         (entries '((:type "message" :id "a" :message (:role "assistant" :content []))
                    (:type "custom" :id "split" :parentId "a")
                    (:type "message" :id "b" :parentId "split"
                     :message (:role "assistant" :content []))
                    (:type "message" :id "alt" :parentId "split"
                     :message (:role "assistant" :content []))))
         (session (list :path "/tmp/boundary.jsonl" :entries (vconcat entries)))
         (branch (pilish--org-view-select (pilish--org-view-branches session) "b")))
    (with-temp-buffer
      (insert (pilish--org-view-render session branch))
      (org-mode)
      (let (kinds)
        (org-map-entries
         (lambda ()
           (when (= 1 (org-outline-level))
             (push (org-entry-get nil "PI_KIND") kinds))))
        (should (equal '("assistant" "event" "assistant") (nreverse kinds)))))))

(ert-deftest pilish-test-org-tool-no-argument-properties ()
  (dolist (name '("bash" "read" "write" "edit" "custom"))
    (let ((text (pilish-test--org-tool-transcript
                 name '(:path "file.py" :content "PAYLOAD" :command "COMMAND") "result")))
      (should-not (string-search ":PI_ARGUMENTS:" text))
      (should (string-search ":PI_TOOL_CALL_ID: call" text)))))

(ert-deftest pilish-test-org-tool-write-source-and-fallback ()
  (dolist (error '(nil t))
    (let ((text (pilish-test--org-tool-transcript
                 "write" '(:path "x.py" :content "print('hello')\n#+end_src\n* fake"
                           :eval "yes")
                 "write status" error)))
      (should (string-search "Tool call: write — x.py" text))
      (should (string-search "#+begin_src python :path \"x.py\" :pi-eval \"yes\" :eval never" text))
      (should (string-search ",#+end_src\n,* fake" text))
      (should-not (string-search ":content " text))
      (should-not (string-search ":PI_ARGUMENTS:" text))
      (with-temp-buffer
        (insert text)
        (org-mode)
        (let ((blocks (org-element-map (org-element-parse-buffer) 'src-block #'identity)))
          (should (= 1 (length blocks)))))))
  (let ((text (pilish-test--org-tool-transcript
               "write" '(:file_path "unknown.xyzzy" :content "UNKNOWN_BODY") "done")))
    (should (string-search "#+begin_example\nUNKNOWN_BODY" text))
    (should-not (string-search "#+begin_src json" text)))
  (let ((text (pilish-test--org-tool-transcript "write" '(:path "x.py") "error" t)))
    (should (string-search "#+begin_src json" text))))

(ert-deftest pilish-test-org-tool-code-navigation-presentations ()
  (dolist (case '(("code_outline" "src/app.py" "grep")
                  ("code_definition" "src/app.py" "python")
                  ("code_definition" "src/app.ts" "typescript")))
    (let ((text (pilish-test--org-tool-transcript
                 (nth 0 case) (list :path (nth 1 case) :line 10) "CODE_BODY")))
      (should (string-search (concat "#+begin_src " (nth 2 case) " :path ") text))
      (should (string-search ":line 10 :eval never\nCODE_BODY" text))
      (should-not (string-search "#+begin_src json" text))
      (should (string-search ":PI_ENTRY_ID: r" text))))
  (dolist (name '("code_outline" "code_definition"))
    (let ((text (pilish-test--org-tool-transcript name '(:path "x.py") "ERROR_BODY" t)))
      (should (string-search "#+begin_example\nERROR_BODY" text))
      (should-not (string-search "#+begin_src grep" text))
      (should-not (string-search "#+begin_src python" text))))
  (should (string-search "#+begin_example\nUNKNOWN_BODY"
                         (pilish-test--org-tool-transcript
                          "code_definition" '(:path "unknown.xyzzy") "UNKNOWN_BODY"))))

(ert-deftest pilish-test-org-tool-presentation-registry ()
  (let* ((calls 0)
         (pilish-org-tool-presentations
          `(("custom"
             :call ,(lambda (call result)
                      (should (equal "call" (plist-get call :id)))
                      (should (equal "r" (plist-get result :id)))
                      (pilish--org-literal-block "CUSTOM_CALL" "bash"))
             :result-language ,(lambda (call result)
                                 (cl-incf calls)
                                 (should (equal "r" (plist-get result :id)))
                                 (pilish--org-tool-language
                                  (pilish--org-tool-path (plist-get call :arguments)))))
            ,@pilish-org-tool-presentations)))
    (let ((text (pilish-test--org-tool-transcript "custom" '(:path "x.py") "CUSTOM_RESULT")))
      (should (string-search "#+begin_src bash :eval never\nCUSTOM_CALL" text))
      (should (string-search "#+begin_src python :path \"x.py\" :eval never\nCUSTOM_RESULT" text))
      (with-temp-buffer
        (insert text)
        (org-mode)
        (should (= 2 (length (org-element-map (org-element-parse-buffer) 'src-block #'identity))))
        (goto-char (point-min))
        (search-forward "*** Result")
        (should (equal "r" (org-entry-get nil "PI_ENTRY_ID")))))
    (pilish-test--org-tool-transcript "custom" '(:path "x.py") "ERROR" t)
    (should (= 1 calls)))
  (let ((pilish-org-tool-presentations '(("custom" :result-language "grep\n#+end_src"))))
    (should (string-search "#+begin_example\nSAFE_BODY"
                           (pilish-test--org-tool-transcript "custom" nil "SAFE_BODY"))))
  (let ((pilish-org-tool-presentations
         (cons '("code_outline" :result-language "sql") pilish-org-tool-presentations)))
    (let ((text (pilish-test--org-tool-transcript "code_outline" '(:path "x.py") "OVERRIDE_BODY")))
      (should (string-search "#+begin_src sql :path " text))
      (should-not (string-search "#+begin_src grep" text))))
  (let ((pilish-org-tool-presentations nil))
    (let ((text (pilish-test--org-tool-transcript "read" '(:path "x.py") "FALLBACK_BODY")))
      (should (string-search "#+begin_src json" text))
      (should (string-search "#+begin_example\nFALLBACK_BODY" text))))
  (with-temp-buffer
    ;; A constant output language also works without a paired call.
    (pilish--org-view-tool-result
     '(:type "message" :id "orphan" :message (:role "toolResult" :toolName "code_outline"
                                            :content "ORPHAN_OUTLINE")))
    (should (string-search "#+begin_src grep :eval never\nORPHAN_OUTLINE" (buffer-string)))))

(ert-deftest pilish-test-org-view-events-and-system-opt-in ()
  (let* ((entries '((:type "message" :id "a" :message (:role "assistant" :content []))
                    (:type "model_change" :id "model" :parentId "a" :modelId "HIDDEN_EVENT")
                    (:type "thinking_level_change" :id "level" :parentId "model" :thinkingLevel "high")
                    (:type "message" :id "system" :parentId "level"
                     :message (:role "system" :content "HIDDEN_SYSTEM"))
                    (:type "message" :id "b" :parentId "system"
                     :message (:role "assistant" :content []))))
         (session (list :path "/tmp/quiet.jsonl" :entries (vconcat entries)))
         (branch (pilish--org-view-select (pilish--org-view-branches session) "b")))
    (dolist (events '(nil t))
      (dolist (system '(nil t))
        (let* ((pilish-org-view-show-events events)
               (pilish-org-view-show-system system)
               (text (pilish--org-view-render session branch)))
          (should (eq (not (null (string-search "HIDDEN_EVENT" text))) events))
          (should (eq (not (null (string-search "HIDDEN_SYSTEM" text))) system))
          ;; Suppression does not stop replaying the original selected path.
          (should (string-search ":PI_THINKING_LEVEL: high" text))
          (with-temp-buffer
            (insert text)
            (org-mode)
            (let ((assistants 0))
              (org-map-entries
               (lambda ()
                 (when (and (= 1 (org-outline-level))
                            (equal "assistant" (org-entry-get nil "PI_KIND")))
                   (cl-incf assistants))))
              (should (= (if (or events system) 2 1) assistants)))))))))

(ert-deftest pilish-test-org-view-omits-hidden-usage-boundaries-by-default ()
  (let* ((entries '((:type "message" :id "a" :message (:role "assistant" :content []))
                    (:type "usage" :id "split" :parentId "a")
                    (:type "message" :id "b" :parentId "split"
                     :message (:role "assistant" :content []))
                    (:type "message" :id "alt" :parentId "split"
                     :message (:role "assistant" :content []))))
         (session (list :path "/tmp/usage-split.jsonl" :entries (vconcat entries)))
         (branch (pilish--org-view-select (pilish--org-view-branches session) "b"))
         (pilish-org-view-show-usage nil))
    (dolist (events '(nil t))
      (let* ((pilish-org-view-show-events events)
             (text (pilish--org-view-render session branch)))
        (should (eq events (not (null (string-search "Branch metadata: usage" text)))))
        (with-temp-buffer
          (insert text)
          (org-mode)
          (goto-char (point-min))
          (search-forward ":PI_ENTRY_ID: b\n")
          (when (= 1 (org-outline-level))
            (search-forward ":PI_ENTRY_ID: b\n"))
          (should (equal "split" (org-entry-get nil "PI_BRANCH_FROM_ID"))))))))

(ert-deftest pilish-test-org-view-startup-aligns-tables-on-load-and-refresh ()
  (pilish-test--org-grammars)
  (let ((file (make-temp-file "pilish-org-align-" nil ".jsonl"))
        (session (pilish-test--org-session))
        (org-startup-align-all-tables nil)
        buffer original)
    (setf (plist-get (plist-get (aref (plist-get session :entries) 0) :message) :content)
          "| A | Longer |\n|---|---|\n| a | b |\n\n```text\n|literal|x|\n```\n")
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (json-encode '(:type "session" :version 3 :id "session")) "\n")
            (seq-doseq (entry (plist-get session :entries))
              (insert (let ((json-null :null)) (json-encode entry)) "\n")))
          (setq original (with-temp-buffer (insert-file-contents-literally file) (buffer-string)))
          (should (string-search "#+STARTUP: align\n"
                                 (pilish--org-view-render
                                  session (pilish--org-view-select
                                           (pilish--org-view-branches session) "b"))))
          (setq buffer (pilish--org-view-load file "b"))
          (with-current-buffer buffer
            (dotimes (_ 2)
              (should org-startup-align-all-tables)
              (should (string-search "| a | b      |" (buffer-string)))
              (should (string-search "|---+--------|" (buffer-string)))
              ;; Align actual Org tables, never archived literal tool/code text.
              (should (string-search "|literal|x|\n" (buffer-string)))
              (should buffer-read-only)
              (should-not (buffer-modified-p))
              (setq-local org-startup-align-all-tables nil)
              (pilish-org-view-refresh)))
          (should (equal original (with-temp-buffer
                                    (insert-file-contents-literally file)
                                    (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file file))))

(provide 'pilish-org-view-test)
;;; pilish-org-view-test.el ends here
