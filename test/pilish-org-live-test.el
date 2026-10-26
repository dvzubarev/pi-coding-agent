;;; pilish-org-live-test.el --- Live Org companion tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional real-converter tests; same external prerequisites as org-view tests.

;;; Code:

(require 'pilish-test-common)
(require 'pilish-org-live)
(require 'pilish)

(defmacro pilish-test--with-org-live (&rest body)
  "Run BODY with an idle fake CHAT, PROCESS and attached VIEW.
REQUESTS records RPC callbacks so tests can also exercise stale replies."
  (declare (indent 0) (debug t))
  `(let* ((chat (generate-new-buffer " *org-live-chat*"))
          (view (generate-new-buffer " *org-live-view*"))
          (process (make-pipe-process :name "org-live-test" :noquery t))
          (state '(:success t :data (:sessionId "session" :sessionFile "/tmp/live.jsonl"
                                    :isStreaming :false :isCompacting :false
                                    :pendingMessageCount 0)))
          (history '(:success t :data (:entries [] :leafId :null)))
          requests)
     (unwind-protect
         (progn
           (unless (and (treesit-language-available-p 'markdown)
                        (treesit-language-available-p 'markdown-inline))
             (ert-skip "Markdown grammars are required"))
           (with-current-buffer chat
             (setq-local pilish--process process)
             (setq-local pilish--status 'idle)
             (setq-local pilish--state (copy-tree '(:session-id "session" :session-file "/tmp/live.jsonl"
                                                   :thinking-level "high"))))
           (process-put process 'pilish-chat-buffer chat)
           (with-current-buffer view
             (let ((org-inhibit-startup t)) (pilish-org-live-mode)))
           (cl-letf (((symbol-function 'pilish--rpc-async)
                      (lambda (_process command callback)
                        (push (cons (plist-get command :type) callback) requests)
                        (funcall callback (if (equal (plist-get command :type) "get_state")
                                              state history)))))
             (pilish--org-live-attach chat view)
             (with-current-buffer view
               (should pilish--org-live-ready)
               ,@body)))
       (when (buffer-live-p view) (kill-buffer view))
       (when (buffer-live-p chat) (kill-buffer chat))
       (delete-process process))))

(defun pilish-test--org-live-emit (process type &rest fields)
  "Send a TYPE event with FIELDS through PROCESS's real observer dispatch."
  (pilish--handle-event process (append (list :type type) fields)))

(defun pilish-test--org-live-update (process type index &rest fields)
  "Emit nested update TYPE at INDEX with FIELDS through PROCESS."
  (pilish-test--org-live-emit
   process "message_update"
   :assistantMessageEvent (append (list :type type :contentIndex index) fields)))

(defun pilish-test--org-live-start (process)
  "Begin an empty assistant response on PROCESS."
  (pilish-test--org-live-emit process "message_start"
                              :message '(:role "assistant" :model "initial" :content [])))

(ert-deftest pilish-test-org-live-observers-single-owner-and-error-isolation ()
  (let* ((process (make-pipe-process :name "org-observer-test" :noquery t))
         (display-count 0) observed notices
         (pilish-event-observer-functions
          (list (lambda (_process event) (push (plist-get event :type) observed))
                (lambda (&rest _) (error "observer failure"))
                (lambda (&rest _) (push 'last observed)))))
    (unwind-protect
        (progn
          (process-put process 'pilish-display-handler
                       (lambda (_event) (cl-incf display-count)))
          (cl-letf (((symbol-function 'message) (lambda (&rest args) (push args notices))))
            (pilish--handle-event process '(:type "extension_ui_request")))
          (should (= display-count 1))
          (should (equal observed '(last "extension_ui_request")))
          (should (= (length notices) 1))
          (process-put process 'pilish-display-handler (lambda (_) (error "display failure")))
          (let ((pilish-event-observer-functions (list (lambda (&rest _) (push 'after-error observed)))))
            (should-error (pilish--handle-event process '(:type "agent_start"))))
          (should (eq (car observed) 'after-error)))
      (delete-process process))))

(ert-deftest pilish-test-org-live-observer-after-consumed-event ()
  (let* ((process (make-pipe-process :name "org-consumed-event" :noquery t))
         (seen nil)
         (pilish-process-event-functions (list (lambda (&rest _) t)))
         (pilish-event-observer-functions (list (lambda (&rest _) (push 'observed seen)))))
    (unwind-protect
        (progn
          (process-put process 'pilish-display-handler
                       (lambda (_) (push 'displayed seen)))
          (pilish--handle-event process '(:type "agent_start"))
          (should (equal seen '(observed))))
      (delete-process process))))

(ert-deftest pilish-test-org-live-throttles-blocks-not-events ()
  (pilish-test--with-org-live
    (let ((count 0) (converter (symbol-function 'pilish-org-markdown-to-org)))
      (cl-letf (((symbol-function 'pilish-org-markdown-to-org)
                 (lambda (&rest args) (cl-incf count) (apply converter args))))
        (pilish-test--org-live-start process)
        (pilish-test--org-live-update process "text_start" 0)
        (let ((timer pilish--org-live-timer))
          (dolist (chunk '("Привет " "**ми" "р**\n\n```python\n" "print(1)\n"))
            (pilish-test--org-live-update process "text_delta" 0 :delta chunk)
            (should (eq timer pilish--org-live-timer)))
          (should (= count 0))
          (pilish--org-live-flush)
          (should (= count 1))
          (should-not pilish--org-live-timer)
          (should (string-search "Привет *мир*" (buffer-string)))
          (should (string-search "#+end_src" (buffer-string)))
          (pilish--org-live-flush)
          (should (= count 1)))))))

(ert-deftest pilish-test-org-live-final-block-and-message-are-authoritative ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 0 :delta "wrong")
    (pilish-test--org-live-update process "text_end" 0 :content "block correction")
    (should (string-search "block correction" (buffer-string)))
    (pilish-test--org-live-update process "thinking_delta" 1 :delta "obsolete block")
    (pilish--org-live-flush)
    ;; A delayed paint must not pick up the UI's subsequently selected level.
    (with-current-buffer chat (setf (plist-get pilish--state :thinking-level) "low"))
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "assistant" :model "final-model"
                                          :content [(:type "text" :text "final correction")]))
    (let ((text (buffer-string)))
      (should (string-search "final correction" text))
      (should-not (string-search "obsolete block" text))
      (should-not (string-search "block correction" text))
      (should (string-search ":PI_MODEL: final-model" text))
      (should (string-search ":PI_THINKING_LEVEL: high" text))
      (should-not (string-search ":PI_ENTRY_ID:" text))
      (should-not (string-search ":PI_CHILD_COUNT:" text)))
    (goto-char (point-max))
    (should-error (pilish-org-live-source) :type 'user-error)))

(ert-deftest pilish-test-org-live-content-index-order-and-kind-correction ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 2 :delta "third block")
    (pilish--org-live-flush)
    (pilish-test--org-live-update process "thinking_delta" 0 :delta "first block")
    (pilish-test--org-live-update process "text_delta" 1 :delta "second block")
    (pilish--org-live-flush)
    (should (< (string-search "first block" (buffer-string))
               (string-search "second block" (buffer-string))
               (string-search "third block" (buffer-string))))
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "assistant" :thinkingLevel "medium"
                                          :content [(:type "text" :text "retyped")]))
    (should (string-search "retyped" (buffer-string)))
    (should-not (string-search "Thinking :" (buffer-string)))
    (should-not (string-search "second block" (buffer-string)))
    (should (string-search ":PI_THINKING_LEVEL: medium" (buffer-string)))))

(ert-deftest pilish-test-org-live-only-accepted-user-messages ()
  (pilish-test--with-org-live
    (pilish-test--org-live-emit process "queue_update" :followUp ["not delivered"])
    (should-not (string-search "not delivered" (buffer-string)))
    (pilish-test--org-live-emit process "message_start"
                               :message '(:role "user" :content "accepted"))
    (should-not (string-search "accepted" (buffer-string)))
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "user" :content "accepted"))
    (should (= 1 (how-many "accepted" (point-min) (point-max))))
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "custom" :display :false :content "secret"))
    (should-not (string-search "secret" (buffer-string)))
    (pilish-test--org-live-emit process "extension_ui_request" :method "input" :title "not our dialog")
    (should-not (string-search "not our dialog" (buffer-string)))))

(defun pilish-test--org-live-call (process id)
  "Complete a bash call with ID on PROCESS."
  (pilish-test--org-live-start process)
  (pilish-test--org-live-update process "toolcall_start" 0 :id id :toolName "bash")
  (pilish-test--org-live-update process "toolcall_delta" 0 :delta "{\"command\":")
  (pilish--org-live-flush)
  (pilish-test--org-live-update process "toolcall_end" 0
                               :toolCall (list :type "toolCall" :id id :name "bash"
                                               :arguments '(:command "echo hello" :eval "yes")))
  (pilish-test--org-live-emit process "message_end"
                             :message (list :role "assistant"
                                            :content (vector (list :type "toolCall" :id id :name "bash"
                                                                   :arguments '(:command "echo hello" :eval "yes"))))))

(ert-deftest pilish-test-org-live-tool-result-deduplication-and-safety ()
  (pilish-test--with-org-live
    (pilish-test--org-live-call process "call")
    (pilish-test--org-live-emit process "tool_execution_update" :toolCallId "call"
                               :partialResult '(:content [(:type "text" :text "contract-dependent")]))
    (should-not (string-search "contract-dependent" (buffer-string)))
    (pilish-test--org-live-emit process "tool_execution_end" :toolCallId "call" :toolName "bash"
                               :isError :false :result '(:content [(:type "text" :text "provisional result")]))
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "toolResult" :toolCallId "call" :toolName "bash"
                                          :isError t :content [(:type "text" :text "* literal\n#+end_example\nfinal result")]))
    (should (= 1 (how-many "^\\*\\*\\* Result" (point-min) (point-max))))
    (should-not (string-search "provisional result" (buffer-string)))
    (should (string-search ",#+end_example" (buffer-string)))
    (should (string-search ":pi-eval \"yes\" :eval never" (buffer-string)))
    (should (string-search ":PI_STATUS: error" (buffer-string)))
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Tool call:")
    (org-back-to-heading)
    (org-narrow-to-subtree)
    (should (string-search "final result" (buffer-string)))
    (widen)
    ;; Sequential reuse must create a second occurrence, not overwrite the first.
    (pilish-test--org-live-call process "call")
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "toolResult" :toolCallId "call" :toolName "bash"
                                          :content [(:type "text" :text "second result")]))
    (should (= 2 (how-many "^\\*\\*\\* Result" (point-min) (point-max))))
    (should (= 1 (how-many "^\\* Assistant" (point-min) (point-max))))
    (should (string-search "final result" (buffer-string)))
    (should (string-search "second result" (buffer-string)))))

(ert-deftest pilish-test-org-live-ambiguous-and-orphan-results-remain-visible ()
  (pilish-test--with-org-live
    (pilish-test--org-live-call process "same")
    (pilish-test--org-live-call process "same")
    (should-not (pilish--org-live-find-call "same"))
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "toolResult" :toolCallId "same" :toolName "bash"
                                          :content [(:type "text" :text "ambiguous visible output")]))
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "toolResult" :toolCallId "orphan" :toolName "read"
                                          :content [(:type "text" :text "orphan visible output")]))
    (should (string-search "ambiguous visible output" (buffer-string)))
    (should (string-search "orphan visible output" (buffer-string)))
    (should-not (string-search "*** Result" (buffer-string)))))

(ert-deftest pilish-test-org-live-body-markers-folds-point-and-narrowing ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "thinking_delta" 0 :delta "private reasoning")
    (pilish-test--org-live-update process "text_delta" 1 :delta "# Stable heading\n\nbody")
    (pilish--org-live-flush)
    (goto-char (point-min))
    (search-forward "private reasoning")
    (should (org-invisible-p))
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Response")
    (org-back-to-heading)
    (org-narrow-to-subtree)
    (let ((minimum (point-min)))
      (goto-char (point-min))
      (search-forward "body")
      (backward-char 2)
      (let ((position (point)))
        ;; Update an off-restriction block and the visible body simultaneously.
        (pilish-test--org-live-update process "thinking_delta" 0 :delta " more private")
        (pilish-test--org-live-update process "text_delta" 1 :delta " more visible")
        (pilish--org-live-flush)
        (should (buffer-narrowed-p))
        (should (> (point-min) minimum))
        (should (looking-at-p "dy more visible"))
        (should (>= (point) position))
        (should-not (string-search "private" (buffer-string)))))
    (widen)
    (goto-char (point-min))
    (search-forward "private reasoning more private")
    (should (org-invisible-p))
    ;; Manually opened thinking must not be refolded by subsequent paints.
    (org-back-to-heading)
    (org-fold-show-subtree)
    (pilish-test--org-live-update process "thinking_delta" 0 :delta " latest")
    (pilish--org-live-flush)
    (search-forward "latest")
    (should-not (org-invisible-p))
    (should buffer-read-only)
    (should-not (buffer-modified-p))
    (should-not buffer-file-name)
    (should (eq buffer-undo-list t))))

(defun pilish-test--org-live-check-drawers (hidden)
  "Check native drawer folding against HIDDEN, independently of outline folds."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^:PROPERTIES:" nil t)
        (cl-incf count)
        ;; Org leaves the opening line visible and folds from its end.
        (should-not (org-fold-folded-p (line-beginning-position) 'drawer))
        (forward-line 1)
        (should (eq (not (null (org-fold-folded-p (point) 'drawer))) hidden)))
      (should (> count 0)))))

(ert-deftest pilish-test-org-live-drawers-survive-command-loop-and-unfolding ()
  "Live drawer folds must survive Org's delayed structural validation."
  (dolist (style '(overlays text-properties))
    (ert-info ((format "folding backend: %s" style))
      (let ((org-fold-core-style style))
        (pilish-test--with-org-live
          (should (eq org-startup-folded 'showall))
          (should org-cycle-hide-drawer-startup)
          (pilish-test--org-live-start process)
          (pilish-test--org-live-update process "thinking_delta" 0 :delta "hidden thinking")
          (pilish-test--org-live-update process "text_delta" 1 :delta "visible response")
          (pilish--org-live-flush)
          ;; A timer paints outside a command.  Switching to the buffer then
          ;; runs post-command-hook and Org's deferred fold validation.
          (save-window-excursion
            (switch-to-buffer view)
            (run-hooks 'post-command-hook)
            (font-lock-ensure)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Response")
            (forward-line 2)
            (should (org-fold-folded-p (point) 'drawer))
            (pilish-test--org-live-check-drawers t)
            (goto-char (point-min))
            (re-search-forward "^\\* Assistant")
            (org-back-to-heading)
            (org-fold-hide-subtree)
            (let ((inhibit-message t))
              (org-cycle)
              (org-cycle))
            (run-hooks 'post-command-hook)
            (pilish-test--org-live-check-drawers t))
          ;; Final metadata edits must not invalidate otherwise valid folds.
          (pilish-test--org-live-emit
           process "message_end"
           :message '(:role "assistant" :model "corrected-model" :thinkingLevel "medium"
                      :content [(:type "thinking" :thinking "final thinking")
                                (:type "text" :text "final response")]))
          (run-hooks 'post-command-hook)
          (pilish-test--org-live-check-drawers t))))))

(ert-deftest pilish-test-org-live-drawers-honor-startup-option ()
  (dolist (option '("nohidedrawers" "showeverything"))
    (ert-info ((format "startup option: %s" option))
      (pilish-test--with-org-live
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (search-forward "hidedrawers")
          (replace-match option))
        (org-set-regexps-and-options)
        (if (equal option "nohidedrawers")
            (should-not org-cycle-hide-drawer-startup)
          (should (eq org-startup-folded 'showeverything)))
        (pilish-test--org-live-emit process "message_end"
                                   :message '(:role "user" :content "drawer should start open"))
        (pilish-test--org-live-check-drawers nil)
        (run-hooks 'post-command-hook)
        (pilish-test--org-live-check-drawers nil)))))

(ert-deftest pilish-test-org-live-manually-opened-drawer-stays-open ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 0 :delta "response")
    (pilish--org-live-flush)
    (run-hooks 'post-command-hook)
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Response")
    (forward-line 1)
    (org-fold-hide-drawer-toggle 'off)
    (pilish-test--org-live-emit
     process "message_end"
     :message '(:role "assistant" :model "new-model"
                :content [(:type "text" :text "corrected response")]))
    (run-hooks 'post-command-hook)
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Response")
    (forward-line 2)
    (should-not (org-fold-folded-p (point) 'drawer))))

(ert-deftest pilish-test-org-live-empty-final-and-babel-protection ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 0 :delta "discarded")
    (pilish--org-live-flush)
    (pilish-test--org-live-emit process "message_end"
                               :message '(:role "assistant" :content [] :errorMessage "aborted"))
    (should-not (string-search "discarded" (buffer-string)))
    (should (string-search "Assistant entry" (buffer-string)))
    (should (string-search "aborted" (buffer-string)))
    (should-not org-export-use-babel)
    (should-error (funcall org-confirm-babel-evaluate) :type 'user-error)))

(ert-deftest pilish-test-org-live-refresh-is-authoritative-offline-render ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 0 :delta "discard provisional")
    (pilish--org-live-flush)
    (setq history '(:success t :data
                    (:entries [(:id "u" :parentId :null :type "message"
                                :message (:role "user" :content "persisted prompt"))
                               (:id "a" :parentId "u" :type "message"
                                :message (:role "assistant" :content [(:type "text" :text "persisted answer")]))
                               (:id "b" :parentId "u" :type "message"
                                :message (:role "assistant" :content [(:type "text" :text "abandoned answer")]))]
                     :leafId "a")))
    (pilish-org-live-refresh)
    (should pilish--org-live-ready)
    (should-not (string-search "discard provisional" (buffer-string)))
    (should-not (string-search "abandoned answer" (buffer-string)))
    (should (string-search ":PI_ENTRY_ID: a" (buffer-string)))
    (should (string-search ":PI_CHILD_COUNT: 2" (buffer-string)))
    (let* ((session (list :path "/tmp/live.jsonl" :header (list :cwd default-directory)
                          :entries (plist-get (plist-get history :data) :entries) :leafId "a"))
           (branch (pilish--org-view-select (pilish--org-view-branches session) "a")))
      (should (equal (buffer-string) (pilish--org-view-render session branch))))
    (should (equal (mapcar #'car requests) '("get_entries" "get_state" "get_entries" "get_state")))))

(ert-deftest pilish-test-org-live-stale-attachment-replies-and-busy-race ()
  (pilish-test--with-org-live
    (let ((old-callback (cdr (assoc "get_entries" requests))))
      (pilish-org-live-refresh)
      (let ((text (buffer-string)))
        (funcall old-callback '(:success t :data (:entries [] :leafId "missing")))
        (should (equal text (buffer-string)))
        (should pilish--org-live-ready)))
    (cl-letf (((symbol-function 'pilish--rpc-async)
               (lambda (_process _command _callback) nil)))
      (pilish-org-live-refresh)
      (should-not pilish--org-live-ready)
      (pilish-test--org-live-emit process "agent_start")
      (should-not pilish--org-live-process)
      (should-not pilish--org-live-guard-timer)
      (should (string-search "activity during attachment" header-line-format)))))

(ert-deftest pilish-test-org-live-null-leaf-does-not-select-abandoned-branch ()
  (pilish-test--with-org-live
    (setq history '(:success t :data
                    (:entries [(:id "old" :parentId :null :type "message"
                                :message (:role "user" :content "abandoned root"))] :leafId :null)))
    (pilish-org-live-refresh)
    (should pilish--org-live-ready)
    (should-not (string-search "abandoned root" (buffer-string)))))

(ert-deftest pilish-test-org-live-ready-survives-outbound-history-invalidation ()
  (pilish-test--with-org-live
    (let ((token pilish--org-live-token))
      ;; The normal input/queued-send path does this before sending a prompt.
      (with-current-buffer chat (pilish--invalidate-history-loads))
      (should (pilish--org-live-current-p))
      (pilish--org-live-guard view token)
      (should (eq process pilish--org-live-process))
      (pilish-test--org-live-start process)
      (pilish-test--org-live-update process "text_delta" 0 :delta "new output after input")
      (pilish--org-live-flush)
      (should pilish--org-live-ready)
      (should (string-search "new output after input" (buffer-string))))))

(ert-deftest pilish-test-org-live-attaching-still-rejects-history-invalidation ()
  (pilish-test--with-org-live
    (let (history-callback)
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (_process command callback)
                   (if (equal (plist-get command :type) "get_state")
                       (funcall callback state)
                     (setq history-callback callback)))))
        (pilish-org-live-refresh)
        (should history-callback)
        (should-not pilish--org-live-ready)
        ;; Even if the chat is idle again before its snapshot reply arrives,
        ;; outbound activity invalidates an in-flight attachment.
        (with-current-buffer chat (pilish--invalidate-history-loads))
        (funcall history-callback history)
        (should-not pilish--org-live-process)
        (should-not pilish--org-live-ready)))))

(ert-deftest pilish-test-org-live-generation-detach-and-cleanup ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 0 :delta "not painted in old generation")
    (let ((token pilish--org-live-token)
          (timer pilish--org-live-timer)
          (sections pilish--org-live-sections))
      (with-current-buffer chat (cl-incf pilish--session-transition-generation))
      (pilish--org-live-guard view token)
      (should-not pilish--org-live-process)
      (should-not (memq timer timer-list))
      (should-not (process-get process 'pilish-org-live-buffers))
      (dolist (section sections)
        (should-not (marker-buffer (pilish--org-live-section-start section))))
      (should-not (string-search "not painted in old generation" (buffer-string))))))

(ert-deftest pilish-test-org-live-exit-and-kill-remove-subscriptions ()
  (pilish-test--with-org-live
    (pilish--org-live-exit process '(:processExit t))
    (should-not pilish--org-live-process)
    (should-not (process-get process 'pilish-org-live-buffers))
    (pilish-org-live-refresh)
    (should pilish--org-live-ready)
    (kill-buffer view)
    (should-not (process-get process 'pilish-org-live-buffers))))

(ert-deftest pilish-test-org-live-sparse-tree-freezes-before-filtering ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "thinking_delta" 0 :delta "hidden reasoning")
    (pilish-test--org-live-update process "text_delta" 1 :delta "visible prose")
    (cl-letf (((symbol-function 'org-sparse-tree)
               (lambda () (interactive)
                 (should-not pilish--org-live-process)
                 (org-match-sparse-tree nil "pi_prose"))))
      (pilish-org-live-sparse-tree))
    (goto-char (point-min))
    (search-forward "visible prose")
    (should-not (org-invisible-p))
    (goto-char (point-min))
    (search-forward "hidden reasoning")
    (should (org-invisible-p))))

(ert-deftest pilish-test-org-live-narrowed-tail-excludes-new-siblings ()
  (pilish-test--with-org-live
    (pilish-test--org-live-call process "first")
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Tool call:")
    (org-back-to-heading)
    (org-narrow-to-subtree)
    (pilish-test--org-live-call process "second")
    (should (buffer-narrowed-p))
    (should (= 1 (how-many "^\\*\\* Tool call:" (point-min) (point-max))))
    (should-not (string-search ":PI_TOOL_CALL_ID: second" (buffer-string)))
    (widen)
    (should (= 2 (how-many "^\\*\\* Tool call:" (point-min) (point-max))))))

(ert-deftest pilish-test-org-live-exit-retains-unpainted-source ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 0 :delta "last tokens before death")
    (with-current-buffer chat (setq pilish--process nil))
    (pilish--org-live-exit process '(:processExit t))
    (should-not pilish--org-live-process)
    (should (string-search "last tokens before death" (buffer-string)))))

(ert-deftest pilish-test-org-live-native-inner-heading-fold-survives-append ()
  (pilish-test--with-org-live
    (pilish-test--org-live-start process)
    (pilish-test--org-live-update process "text_delta" 0 :delta "# Inner heading\n\ninitial body")
    (pilish--org-live-flush)
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* Inner heading")
    (org-back-to-heading)
    (org-fold-hide-subtree)
    (pilish-test--org-live-update process "text_delta" 0 :delta " appended body")
    (pilish--org-live-flush)
    (search-forward "initial body")
    (should (org-invisible-p))))

(ert-deftest pilish-test-org-live-timer-paints-without-a-boundary-event ()
  (pilish-test--with-org-live
    (let ((pilish-org-live-paint-interval 0.02))
      (pilish-test--org-live-start process)
      (pilish-test--org-live-update process "text_delta" 0 :delta "paint before finalization")
      (should (pilish-test-wait-until
               (lambda () (string-search "paint before finalization" (buffer-string)))
               2 0.01 process))
      (should-not pilish--org-live-timer)
      (should-not pilish--org-live-dirty-blocks))))

(ert-deftest pilish-test-org-live-reading-and-following-windows ()
  (pilish-test--with-org-live
    (pilish-test--org-live-emit
     process "message_end" :message
     (list :role "user" :content (mapconcat (lambda (n) (format "Old paragraph %d.\n" n))
                                            (number-sequence 1 40) "\n")))
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view)
      (let* ((reader (selected-window))
             (tail (split-window-below)))
        (goto-char (point-min))
        (search-forward "Old paragraph 3.")
        (set-window-start reader (line-beginning-position) t)
        (set-window-point reader (point))
        (set-window-buffer tail view)
        (set-window-point tail (point-max))
        (let ((read-point (window-point reader))
              (read-start (window-start reader)))
          (pilish-test--org-live-start process)
          (pilish-test--org-live-update process "text_delta" 0 :delta "new streaming output")
          (pilish--org-live-flush)
          (should (= read-point (window-point reader)))
          (should (= read-start (window-start reader)))
          (should (= (window-point tail) (point-max))))))))

(ert-deftest pilish-test-org-live-fake-subprocess-end-to-end ()
  "The companion observes the real primary handler without replacing it."
  (unless (and (treesit-language-available-p 'markdown)
               (treesit-language-available-p 'markdown-inline))
    (ert-skip "Markdown grammars are required"))
  (let* ((directory (file-name-as-directory (make-temp-file "pilish-org-live-" t)))
         (default-directory directory)
         (pilish-executable (pilish-test-fake-pi-executable))
         (pilish-extra-args (list "--scenario" "tool-read" "--session-dir" directory))
         chat input process view)
    (unwind-protect
        (progn
          (setq chat (pilish--setup-session directory)
                input (buffer-local-value 'pilish--input-buffer chat)
                process (buffer-local-value 'pilish--process chat))
          (should (pilish-test-wait-until
                   (lambda () (with-current-buffer chat
                                (and (plist-get pilish--state :session-id)
                                     (not (pilish--session-busy-p)))))
                   5 0.01 process))
          (setq view (generate-new-buffer " *org-live-subprocess*"))
          (with-current-buffer view
            (let ((org-inhibit-startup t)) (pilish-org-live-mode)))
          (pilish--org-live-attach chat view)
          (should (pilish-test-wait-until
                   (lambda () (buffer-local-value 'pilish--org-live-ready view))
                   5 0.01 process))
          (let ((primary (process-get process 'pilish-display-handler)))
            ;; Exercise the public input path, not just its low-level RPC send:
            ;; pilish--prepare-and-send also invalidates pending Markdown history.
            (let ((generation (buffer-local-value 'pilish--history-load-generation chat)))
              (with-current-buffer input
                (insert "Read the example")
                (pilish-send)
                (should (string-empty-p (buffer-string))))
              (should (> (buffer-local-value 'pilish--history-load-generation chat)
                         generation)))
            (should (pilish-test-wait-until
                     (lambda ()
                       (and (with-current-buffer chat
                              (and (eq pilish--status 'idle) (not (pilish--session-busy-p))))
                            (with-current-buffer view
                              (string-search "Tool finished" (buffer-string)))))
                     5 0.01 process))
            (should (eq primary (process-get process 'pilish-display-handler))))
          (with-current-buffer chat
            (should (string-search "Tool finished" (buffer-string))))
          (with-current-buffer view
            (should pilish--org-live-ready)
            (should (= 1 (how-many "^\\* User" (point-min) (point-max))))
            (should (= 1 (how-many "^\\* Assistant" (point-min) (point-max))))
            (should (= 1 (how-many "^\\*\\*\\* Result" (point-min) (point-max))))
            (should (= 1 (how-many "more output" (point-min) (point-max))))
            (let* ((path pilish--org-view-path)
                   (digest (with-temp-buffer (insert-file-contents-literally path)
                                             (secure-hash 'sha256 (current-buffer)))))
              (pilish-org-live-refresh)
              (should (pilish-test-wait-until
                       (lambda () (buffer-local-value 'pilish--org-live-ready view))
                       5 0.01 process))
              (should (equal digest (with-temp-buffer (insert-file-contents-literally path)
                                                      (secure-hash 'sha256 (current-buffer)))))
              (let* ((session (pilish-jsonl-read-file path))
                     (branch (pilish--org-view-select (pilish--org-view-branches session)
                                                      (plist-get session :leafId))))
                (should (equal (buffer-string) (pilish--org-view-render session branch)))))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (and process (process-live-p process)) (delete-process process))
      (pilish-test--kill-live-buffers input chat)
      (delete-directory directory t))))

(provide 'pilish-org-live-test)
;;; pilish-org-live-test.el ends here
