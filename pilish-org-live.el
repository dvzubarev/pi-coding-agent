;;; pilish-org-live.el --- Optional live Org companion for Pilish -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; M-x pilish-org-live from an idle Pilish session.  Interaction stays in the
;; ordinary Pilish buffers.  This projection observes, but never owns, lifecycle
;; events.  Initial history comes from get_entries, not get_messages.
;;
;; First slice: streamed text/thinking, completed tool calls/results, and an
;; explicit idle refresh for persisted identities/graph metadata.  Unknown tool
;; partial-result contracts are deliberately not interpreted.  Busy attachment
;; fails closed instead of merging potentially overlapping snapshots/events.

;;; Code:

(require 'cl-lib)
(require 'pilish-ui)
(require 'pilish-org-live-buffer)

(defgroup pilish-org-live nil
  "Live read-only Org companions for Pilish."
  :group 'pilish-org-view)

(defcustom pilish-org-live-paint-interval 0.1
  "Minimum delay in seconds before painting accumulated dirty blocks.
The first dirty event schedules a one-shot timer; later deltas do not reset it."
  :type 'number
  :group 'pilish-org-live)

(cl-defstruct (pilish--org-live-block
               (:constructor pilish--org-live-block-create))
  "One content-index-scoped live block and its optional paired result."
  index content entry section dirty result result-section result-seen arguments)

(defvar-local pilish--org-live-process nil "Observed process, or nil when detached.")
(defvar-local pilish--org-live-chat nil "Owning Pilish chat buffer.")
(defvar-local pilish--org-live-generation nil
  "Captured transition generation and history-load generation at attachment.
The history-load generation guards initial attachment only, not live delivery.")
(defvar-local pilish--org-live-session nil "Captured session identity (ID and file).")
(defvar-local pilish--org-live-ready nil "Whether idle history attachment completed.")
(defvar-local pilish--org-live-token nil "Unique attachment token for delayed callbacks.")
(defvar-local pilish--org-live-deadline nil "Deadline for initial history attachment.")
(defvar-local pilish--org-live-section-serial 0 "Monotonic provisional section counter.")
(defvar-local pilish--org-live-timer nil "Pending one-shot paint timer.")
(defvar-local pilish--org-live-guard-timer nil "Ownership/death guard timer.")
(defvar-local pilish--org-live-serial 0 "Provisional message sequence in this attachment.")
(defvar-local pilish--org-live-entry nil "Current provisional assistant entry.")
(defvar-local pilish--org-live-blocks nil "Current assistant blocks, keyed by content index.")
(defvar-local pilish--org-live-all-blocks nil "Current blocks and unresolved tool calls.")
(defvar-local pilish--org-live-dirty-blocks nil "Blocks awaiting a paint, newest first.")
(defvar-local pilish--org-live-sections nil "All allocated section marker sets.")
(defvar-local pilish--org-live-group-open nil "Whether an assistant container is open.")

(defun pilish--org-live-current-p ()
  "Return non-nil if this companion still belongs to its original session.
History-load invalidation rejects an in-flight attachment, but ordinary sends
may invalidate Markdown history loads after this companion is ready."
  (let ((process pilish--org-live-process)
        (chat pilish--org-live-chat)
        (generation pilish--org-live-generation)
        (ready pilish--org-live-ready)
        (session pilish--org-live-session))
    (and process (process-live-p process) (buffer-live-p chat)
         (eq (process-get process 'pilish-chat-buffer) chat)
         (with-current-buffer chat
           (and (eq pilish--process process)
                (equal (car generation) pilish--session-transition-generation)
                (or ready
                    (equal (cadr generation) pilish--history-load-generation))
                (equal session (list (plist-get pilish--state :session-id)
                                     (plist-get pilish--state :session-file))))))))

(defun pilish--org-live-idle-p ()
  "Return non-nil if the owning Pilish chat is safe for a history snapshot."
  (and (pilish--org-live-current-p)
       (with-current-buffer pilish--org-live-chat
         (and (eq pilish--status 'idle)
              (not (pilish--session-busy-p))
              (not pilish--local-user-message)))))

(defun pilish-org-live-detach ()
  "Stop observing, cancel timers, and retain this read-only Org snapshot."
  (interactive)
  (when (and (called-interactively-p 'interactive)
             pilish--org-live-ready (pilish--org-live-current-p))
    (pilish--org-live-flush))
  (when pilish--org-live-process
    (process-put pilish--org-live-process 'pilish-org-live-buffers
                 (delq (current-buffer)
                       (process-get pilish--org-live-process
                                    'pilish-org-live-buffers))))
  (dolist (timer (list pilish--org-live-timer pilish--org-live-guard-timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq pilish--org-live-process nil
        pilish--org-live-ready nil
        pilish--org-live-token nil
        pilish--org-live-timer nil
        pilish--org-live-guard-timer nil)
  (dolist (section pilish--org-live-sections)
    (pilish--org-live-release-section section))
  (setq pilish--org-live-sections nil
        pilish--org-live-blocks nil
        pilish--org-live-all-blocks nil
        pilish--org-live-dirty-blocks nil
        pilish--org-live-entry nil)
  (setq header-line-format "Pi Org: detached snapshot (g: reattach while idle)"))

(defun pilish--org-live-stop (reason)
  "Detach this companion with a visible REASON."
  (pilish-org-live-detach)
  (setq header-line-format (concat "Pi Org: detached — " reason)))

(defun pilish--org-live-guard (buffer token)
  "Detach BUFFER if attachment TOKEN expired or lost ownership."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq token pilish--org-live-token)
        (cond
         ((not (pilish--org-live-current-p))
          (pilish--org-live-stop "session changed or process exited"))
         ((and (not pilish--org-live-ready)
               (> (float-time) pilish--org-live-deadline))
          (pilish--org-live-stop "attachment timed out; retry while idle")))))))

(defun pilish--org-live-add-section (position title entry level folded kind &optional properties)
  "Insert and remember a section at POSITION with TITLE and ENTRY.
LEVEL, FOLDED, KIND and PROPERTIES describe its Org presentation."
  (let ((section (pilish--org-live-insert-section
                  position title entry level folded
                  (append `(("PI_LIVE_ID" . ,(format "%s/%s/%d/%d"
                                                     pilish--org-live-session
                                                     pilish--org-live-token
                                                     pilish--org-live-serial
                                                     (cl-incf pilish--org-live-section-serial))))
                          properties)
                  kind)))
    (push section pilish--org-live-sections)
    section))

(defun pilish--org-live-next-position (block)
  "Return the insertion position for BLOCK in current message order."
  (let (next)
    (maphash (lambda (index candidate)
               (when (and (> index (pilish--org-live-block-index block))
                          (pilish--org-live-block-section candidate)
                          (or (null next) (< index (car next))))
                 (setq next (cons index candidate))))
             pilish--org-live-blocks)
    (if next
        (marker-position (pilish--org-live-section-start
                          (pilish--org-live-block-section (cdr next))))
      (point-max))))

(defun pilish--org-live-body (block)
  "Convert BLOCK's complete accumulated content to an Org body."
  (let* ((content (pilish--org-live-block-content block))
         (type (plist-get content :type)))
    (pcase type
      ("text" (pilish-org-markdown-to-org (or (plist-get content :text) "") 2))
      ("thinking" (pilish-org-markdown-to-org (or (plist-get content :thinking) "[Redacted]") 2))
      ("toolCall"
       (or (and (null (pilish--org-live-block-arguments block))
                (pilish--org-tool-call-body content (pilish--org-live-block-result block)))
           (pilish--org-literal-block
            (or (pilish--org-live-block-arguments block)
                (pilish--org-view-json (plist-get content :arguments))) "json")))
      (_ (with-temp-buffer
           (pilish--org-view-content (vector content) nil 2)
           (buffer-string))))))

(defun pilish--org-live-result-body (block)
  "Render BLOCK's latest authoritative tool result using viewer safety rules."
  (let* ((entry (pilish--org-live-block-result block))
         (call (pilish--org-live-block-content block))
         (language (pilish--org-tool-result-language call entry)))
    (with-temp-buffer
      (pilish--org-view-content
       (plist-get (plist-get entry :message) :content) t nil nil language
       (and language (pilish--org-tool-headers (plist-get call :arguments))))
      (buffer-string))))

(defun pilish--org-live-paint-block (block)
  "Paint dirty BLOCK, keeping its heading and paired result stable."
  (let* ((content (pilish--org-live-block-content block))
         (type (plist-get content :type))
         (tool (equal type "toolCall"))
         (entry (pilish--org-live-block-entry block))
         (message (plist-get entry :message))
         (section (pilish--org-live-block-section block))
         (result (pilish--org-live-block-result block)))
    (unless (and (equal type "text")
                 (not section)
                 (not (string-match-p "\\S-" (or (plist-get content :text) ""))))
      (unless section
        (setq section
              (pilish--org-live-add-section
               (pilish--org-live-next-position block)
               (pcase type ("text" "Response") ("thinking" "Thinking")
                      ("toolCall" (concat "Tool call: " (or (plist-get content :name) "unknown")))
                      (_ "Content"))
               entry 2 (member type '("thinking" "toolCall"))
               (pcase type ("text" "response") ("thinking" "thinking")
                      ("toolCall" "tool") (_ "assistant"))
               (when tool `(("PI_TOOL_NAME" . ,(plist-get content :name))))))
        (setf (pilish--org-live-block-section block) section))
      (pilish--org-live-section-properties
       section `(("PI_MODEL" . ,(pilish--org-view-string (plist-get message :model)))
                 ("PI_THINKING_LEVEL" . ,(pilish--org-view-string (plist-get message :thinkingLevel)))))
      (when tool
        (pilish--org-live-section-properties
         section `(("PI_TOOL_CALL_ID" . ,(plist-get content :id))
                   ("PI_TOOL_NAME" . ,(plist-get content :name))
                   ("PI_TOOL_PATH" . ,(pilish--org-tool-path (plist-get content :arguments)))
                   ("PI_STATUS" . ,(cond ((null result) "pending")
                                          ((eq (plist-get (plist-get result :message) :isError) t) "error")
                                          (t "ok"))))))
      (pilish--org-live-update-body section (pilish--org-live-body block))
      (when result
        (unless (pilish--org-live-block-result-section block)
          (setf (pilish--org-live-block-result-section block)
                (pilish--org-live-add-section
                 (pilish--org-live-section-end section) "Result" result 3 nil "tool_result"
                 `(("PI_TOOL_NAME" . ,(plist-get content :name))
                   ("PI_TOOL_CALL_ID" . ,(plist-get content :id))))))
        (pilish--org-live-section-properties
         (pilish--org-live-block-result-section block)
         `(("PI_STATUS" . ,(if (eq (plist-get (plist-get result :message) :isError) t)
                                  "error" "ok"))))
        (pilish--org-live-update-body (pilish--org-live-block-result-section block)
                                      (pilish--org-live-result-body block))))
    (setf (pilish--org-live-block-dirty block) nil)))

(defun pilish--org-live-dirty (block)
  "Mark BLOCK dirty once and enqueue it without scanning transcript history."
  (unless (pilish--org-live-block-dirty block)
    (setf (pilish--org-live-block-dirty block) t)
    (push block pilish--org-live-dirty-blocks)))

(defun pilish--org-live-paint-dirty ()
  "Paint the dirty queue, touching no completed transcript bodies."
  (pilish--org-live-change
   (lambda ()
     (dolist (block (nreverse pilish--org-live-dirty-blocks))
       (pilish--org-live-paint-block block))
     (setq pilish--org-live-dirty-blocks nil))))

(defun pilish--org-live-flush ()
  "Flush dirty blocks once, without reparsing completed transcript history."
  (when (timerp pilish--org-live-timer) (cancel-timer pilish--org-live-timer))
  (setq pilish--org-live-timer nil)
  (when pilish--org-live-process
    (if (not (pilish--org-live-current-p))
        (pilish--org-live-stop "session changed")
      (pilish--org-live-paint-dirty))))

(defun pilish--org-live-schedule ()
  "Schedule one paint without postponing a previously scheduled paint."
  (unless pilish--org-live-timer
    (let ((buffer (current-buffer)) (token pilish--org-live-token))
      (setq pilish--org-live-timer
            (run-at-time
             (max 0.01 pilish-org-live-paint-interval) nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (eq token pilish--org-live-token)
                     (condition-case err
                         (pilish--org-live-flush)
                       (error (pilish--org-live-stop (error-message-string err)))))))))))))

(defun pilish--org-live-remove-block (block)
  "Remove BLOCK's presentation and correlation state."
  (dolist (section (list (pilish--org-live-block-result-section block)
                        (pilish--org-live-block-section block)))
    (when section
      (pilish--org-live-remove-section section)
      (setq pilish--org-live-sections (delq section pilish--org-live-sections))))
  (setq pilish--org-live-all-blocks (delq block pilish--org-live-all-blocks)
        pilish--org-live-dirty-blocks (delq block pilish--org-live-dirty-blocks)))

(defun pilish--org-live-block (index content)
  "Get or create current INDEX with replacement CONTENT."
  (unless (and pilish--org-live-entry (integerp index) (>= index 0))
    (error "Content update outside an assistant message"))
  (let ((block (gethash index pilish--org-live-blocks)))
    (when (and block (not (equal (plist-get content :type)
                                 (plist-get (pilish--org-live-block-content block) :type))))
      (pilish--org-live-change (lambda () (pilish--org-live-remove-block block)))
      (setq block nil))
    (unless block
      (setq block (pilish--org-live-block-create
                   :index index :entry pilish--org-live-entry))
      (puthash index block pilish--org-live-blocks)
      (push block pilish--org-live-all-blocks))
    (setf (pilish--org-live-block-content block) (copy-tree content t))
    (pilish--org-live-dirty block)
    block))

(defun pilish--org-live-start-assistant (message)
  "Begin a provisional assistant MESSAGE, capturing its thinking setting now."
  (pilish--org-live-flush)
  (cl-incf pilish--org-live-serial)
  (let ((message (copy-tree message t)))
    (unless (pilish--org-view-string (plist-get message :thinkingLevel))
      (setq message
            (plist-put message :thinkingLevel
                       (with-current-buffer pilish--org-live-chat
                         (plist-get pilish--state :thinking-level)))))
    (setq pilish--org-live-entry (list :type "message" :message message)
          pilish--org-live-blocks (make-hash-table :test #'eql)))
  (pilish--org-live-change
   (lambda ()
     (unless pilish--org-live-group-open
       (pilish--org-live-add-section (point-max) "Assistant" nil 1 nil "assistant"
                                     '(("PI_ROLE" . "assistant")))
       (setq pilish--org-live-group-open t))))
  (cl-loop for content across (vconcat (plist-get message :content))
           for index from 0 do (pilish--org-live-block index content))
  (pilish--org-live-schedule))

(defun pilish--org-live-update (update)
  "Accumulate one delta-only assistant UPDATE using its content index."
  (let* ((type (plist-get update :type))
         (index (plist-get update :contentIndex))
         (block (and (integerp index) pilish--org-live-blocks
                     (gethash index pilish--org-live-blocks)))
         (content (and block (copy-tree (pilish--org-live-block-content block) t))))
    (pcase type
      ((or "text_start" "thinking_start")
       (pilish--org-live-block index
                               (list :type (if (equal type "text_start") "text" "thinking"))))
      ((or "text_delta" "thinking_delta" "text_end" "thinking_end")
       (let* ((thinking (string-prefix-p "thinking" type))
              (key (if thinking :thinking :text))
              (final (string-suffix-p "_end" type)))
         (setq content (plist-put content :type (if thinking "thinking" "text"))
               content (plist-put content key
                                  (if final (plist-get update :content)
                                    (concat (plist-get content key) (plist-get update :delta)))))
         (pilish--org-live-block index content)))
      ("toolcall_start"
       (pilish--org-live-block index (list :type "toolCall" :id (plist-get update :id)
                                           :name (plist-get update :toolName))))
      ("toolcall_delta"
       (unless block (error "Tool argument delta without a call start"))
       (setf (pilish--org-live-block-arguments block)
             (concat (pilish--org-live-block-arguments block) (plist-get update :delta)))
       (pilish--org-live-dirty block))
      ("toolcall_end"
       (setq block (pilish--org-live-block index (plist-get update :toolCall)))
       (setf (pilish--org-live-block-arguments block) nil)))
    (if (member type '("text_end" "thinking_end" "toolcall_end"))
        (pilish--org-live-flush)
      (pilish--org-live-schedule))))

(defun pilish--org-live-finish-assistant (message)
  "Reconcile current blocks against authoritative final MESSAGE."
  (unless pilish--org-live-entry (pilish--org-live-start-assistant message))
  (let* ((message (copy-tree message t))
         (content (vconcat (plist-get message :content)))
         obsolete)
    (unless (pilish--org-view-string (plist-get message :thinkingLevel))
      (setq message (plist-put message :thinkingLevel
                               (plist-get (plist-get pilish--org-live-entry :message)
                                          :thinkingLevel))))
    (setf (plist-get pilish--org-live-entry :message) message)
    (maphash (lambda (index block)
               (when (or (>= index (length content))
                         (and (equal (plist-get (aref content index) :type) "text")
                              (not (string-match-p "\\S-" (or (plist-get (aref content index) :text) "")))))
                 (push (cons index block) obsolete)))
             pilish--org-live-blocks)
    (pilish--org-live-change
     (lambda ()
       (dolist (pair obsolete)
         (pilish--org-live-remove-block (cdr pair))
         (remhash (car pair) pilish--org-live-blocks))))
    (cl-loop for item across content for index from 0
             unless (and (equal (plist-get item :type) "text")
                         (not (string-match-p "\\S-" (or (plist-get item :text) ""))))
             do (let ((block (pilish--org-live-block index item)))
                  (setf (pilish--org-live-block-arguments block) nil)))
    (pilish--org-live-flush)
    (pilish--org-live-change
     (lambda ()
       (when (= (hash-table-count pilish--org-live-blocks) 0)
         (pilish--org-live-add-section (point-max) "Assistant entry"
                                       pilish--org-live-entry 2 nil "assistant"))
       (when-let* ((error (plist-get message :errorMessage)))
         (let ((section (pilish--org-live-add-section
                         (point-max) "Error" pilish--org-live-entry 2 nil "error")))
           (pilish--org-live-update-body section (pilish--org-literal-block error))))
       (when (and pilish-org-view-show-usage (plist-get message :usage))
         (let ((section (pilish--org-live-add-section
                         (point-max) "Usage" pilish--org-live-entry 2 t "usage")))
           (pilish--org-live-update-body
            section (pilish--org-literal-block
                     (pilish--org-view-json (plist-get message :usage)) "json"))))))
    ;; Only unresolved calls need persistent correlation records.  Completed
    ;; prose retains its Org text/markers, not another copy of its source.
    (setq pilish--org-live-all-blocks
          (cl-remove-if-not
           (lambda (block)
             (and (equal (plist-get (pilish--org-live-block-content block) :type) "toolCall")
                  (not (pilish--org-live-block-result-seen block))))
           pilish--org-live-all-blocks)
          pilish--org-live-blocks nil
          pilish--org-live-entry nil)))

(defun pilish--org-live-find-call (id)
  "Return the unique unresolved occurrence of tool call ID, or nil."
  (let ((matches (cl-remove-if-not
                  (lambda (block)
                    (and (equal (plist-get (pilish--org-live-block-content block) :type) "toolCall")
                         (equal id (plist-get (pilish--org-live-block-content block) :id))
                         (not (pilish--org-live-block-result-seen block))))
                  pilish--org-live-all-blocks)))
    (when (= (length matches) 1) (car matches))))

(defun pilish--org-live-static-message (message)
  "Append a completed non-assistant MESSAGE without speculative prompt echoes."
  (let ((role (plist-get message :role)))
    (unless (or (and (equal role "system") (not pilish-org-view-show-system))
                (and (equal role "custom") (not (eq (plist-get message :display) t))))
      (pilish--org-live-flush)
      (cl-incf pilish--org-live-serial)
      (pilish--org-live-change
       (lambda ()
         (let ((section (pilish--org-live-add-section
                         (point-max) (capitalize (or role "Message"))
                         (list :type "message" :message message) 1
                         (equal role "system")
                         (pcase role ("toolResult" "tool_result")
                                ("bashExecution" "shell")
                                ((or "branchSummary" "compactionSummary") "summary")
                                (_ role)))))
           (pilish--org-live-update-body
            section (with-temp-buffer
                      (if (equal role "bashExecution")
                          (insert (pilish--org-literal-block (or (plist-get message :command) "") "bash")
                                  "\n\n" (pilish--org-literal-block (or (plist-get message :output) "")))
                        (pilish--org-view-content (or (plist-get message :content)
                                                     (plist-get message :summary))
                                                  (equal role "toolResult") 1))
                      (buffer-string))))))
      (setq pilish--org-live-group-open nil))))

(defun pilish--org-live-event (event)
  "Project EVENT without running any Pilish lifecycle or interaction handlers."
  (pcase (plist-get event :type)
    ("message_start"
     (let ((message (plist-get event :message)))
       (when (equal (plist-get message :role) "assistant")
         (pilish--org-live-start-assistant message))))
    ("message_update" (pilish--org-live-update (plist-get event :assistantMessageEvent)))
    ("message_end"
     (let* ((message (plist-get event :message))
            (role (plist-get message :role)))
       (cond
        ((equal role "assistant") (pilish--org-live-finish-assistant message))
        ((equal role "toolResult")
         (if-let* ((block (pilish--org-live-find-call (plist-get message :toolCallId))))
             (progn
               (setf (pilish--org-live-block-result block) (list :type "message" :message (copy-tree message t))
                     (pilish--org-live-block-result-seen block) t)
               (pilish--org-live-dirty block)
               (pilish--org-live-flush)
               (setq pilish--org-live-all-blocks (delq block pilish--org-live-all-blocks)))
           (pilish--org-live-static-message message)))
        (t (pilish--org-live-static-message message)))))
    ("tool_execution_end"
     ;; Nested execution IDs live in another namespace.  Persisted root results
     ;; remain authoritative and may include nested-call details.
     (unless (plist-member event :parentToolCallId)
       (when-let* ((block (pilish--org-live-find-call (plist-get event :toolCallId))))
         (let ((message (copy-tree (plist-get event :result) t)))
           (setq message (plist-put message :role "toolResult")
                 message (plist-put message :toolName (plist-get event :toolName))
                 message (plist-put message :toolCallId (plist-get event :toolCallId))
                 message (plist-put message :isError (plist-get event :isError)))
           (setf (pilish--org-live-block-result block) (list :type "message" :message message))
           (pilish--org-live-dirty block)
           (pilish--org-live-flush)))))
    ((or "agent_end" "agent_settled") (pilish--org-live-flush))))

(defun pilish--org-live-observe (process event)
  "Deliver PROCESS's EVENT to its still-current companions."
  (dolist (buffer (copy-sequence (process-get process 'pilish-org-live-buffers)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process pilish--org-live-process)
          (condition-case err
              (cond
               ((not (pilish--org-live-current-p)) (pilish--org-live-stop "session changed"))
               ((not pilish--org-live-ready)
                ;; No ambiguous snapshot/event merge during idle-only attach.
                (unless (member (plist-get event :type)
                                '("queue_update" "session_info_changed" "extension_ui_request"))
                  (pilish--org-live-stop "activity during attachment; retry while idle")))
               (t (pilish--org-live-event event)))
            (error (pilish--org-live-stop (error-message-string err)))))))))

(defun pilish--org-live-exit (process _response)
  "Freeze and detach PROCESS's companions on process exit."
  (dolist (buffer (copy-sequence (process-get process 'pilish-org-live-buffers)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process pilish--org-live-process)
          ;; Events already accumulated before death still belong to this
          ;; snapshot, even though the primary exit handler cleared ownership.
          (unwind-protect
              (condition-case nil (pilish--org-live-paint-dirty) (error nil))
            (pilish--org-live-stop "process exited")))))))

(defun pilish--org-live-load-history (response)
  "Install an idle get_entries RESPONSE into this newly attached companion."
  (unless (eq (plist-get response :success) t)
    (error "History request failed: %s" (plist-get response :error)))
  (unless (pilish--org-live-idle-p) (error "Session became busy during attachment"))
  (let* ((data (plist-get response :data))
         (session (list :path (or (cadr pilish--org-live-session) "(unsaved session)")
                        :header (list :cwd default-directory)
                        :entries (plist-get data :entries)
                        :leafId (plist-get data :leafId)))
         (leaf (plist-get data :leafId))
         ;; An explicitly null leaf is an empty selected path, not an invitation
         ;; to choose a terminal branch from abandoned history.
         (branch (if (or (null leaf) (eq leaf :null))
                     (list :key nil :entries nil)
                   (pilish--org-view-select (pilish--org-view-branches session) leaf)))
         (text (pilish--org-view-render session branch)))
    (pilish--org-live-change
     (lambda ()
       (erase-buffer)
       (insert text)
       (setq pilish--org-view-leaf (plist-get branch :key))
       (org-set-regexps-and-options)
       (when org-startup-align-all-tables (org-table-map-tables #'org-table-align t 'org))
       (org-cycle-set-startup-visibility)
       (goto-char (point-max))
       (setq pilish--org-live-group-open
             (save-excursion
               (when (re-search-backward org-heading-regexp nil t)
                 (while (> (org-outline-level) 1) (org-up-heading-safe))
                 (equal (org-entry-get nil "PI_KIND") "assistant"))))))
    (setq pilish--org-live-ready t
          header-line-format "Pi Org: live companion (C-c C-d: detach; g: idle refresh)")))

(defun pilish--org-live-attach (chat buffer)
  "Attach BUFFER to idle CHAT, failing closed on snapshot races."
  (unless (fboundp 'pilish--notify-observers)
    (user-error "Reload the updated pilish-core.el before attaching a companion"))
  (let ((process (buffer-local-value 'pilish--process chat))
        (token (gensym "org-live-")))
    (with-current-buffer buffer
      (pilish-org-live-detach)
      (setq pilish--org-live-chat chat
            pilish--org-live-token token
            pilish--org-live-deadline (+ (float-time) 15)
            pilish--org-live-process process
            pilish--org-live-session
            (with-current-buffer chat
              (list (plist-get pilish--state :session-id)
                    (plist-get pilish--state :session-file)))
            pilish--org-live-generation
            (with-current-buffer chat
              (list pilish--session-transition-generation pilish--history-load-generation)))
      (unless (pilish--org-live-idle-p)
        (pilish-org-live-detach)
        (user-error "Attach the Org companion while Pilish is idle"))
      (setq default-directory (buffer-local-value 'default-directory chat)
            pilish--org-view-path (cadr pilish--org-live-session)
            pilish--org-live-serial 0
            header-line-format "Pi Org: attaching…")
      (process-put process 'pilish-org-live-buffers
                   (cons buffer (process-get process 'pilish-org-live-buffers)))
      (setq pilish--org-live-guard-timer
            (run-at-time 1 1 #'pilish--org-live-guard buffer token))
      (condition-case err
          (pilish--rpc-async
           process '(:type "get_state")
           (lambda (response)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq token pilish--org-live-token)
                   (condition-case err
                       (let ((state (pilish--extract-state-from-response response default-directory))
                             (data (plist-get response :data)))
                         (unless (and (eq (plist-get response :success) t)
                                      (pilish--org-live-idle-p)
                                      (not (eq (plist-get data :isStreaming) t))
                                      (not (eq (plist-get data :isCompacting) t))
                                      (zerop (or (plist-get data :pendingMessageCount) 0))
                                      (equal pilish--org-live-session
                                             (list (plist-get state :session-id)
                                                   (plist-get state :session-file))))
                           (error "Session is busy or changed; retry while idle"))
                         (pilish--rpc-async
                          process '(:type "get_entries")
                          (lambda (history)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (when (eq token pilish--org-live-token)
                                  (condition-case err
                                      (pilish--org-live-load-history history)
                                    (error (pilish--org-live-stop (error-message-string err))))))))))
                     (error (pilish--org-live-stop (error-message-string err)))))))))
        (error (pilish--org-live-stop (error-message-string err)))))
    buffer))

(defun pilish-org-live-refresh ()
  "Explicitly reload authoritative raw history, while the owning chat is idle.
This resets folds like the offline viewer.  Live-only provisional sections are
replaced, not matched by text against persisted entries."
  (interactive)
  (unless (buffer-live-p pilish--org-live-chat) (user-error "Original Pilish chat no longer exists"))
  (with-current-buffer pilish--org-live-chat
    (unless (and (eq pilish--status 'idle) (not (pilish--session-busy-p)))
      (user-error "Refresh the Org companion while Pilish is idle")))
  (pilish--org-live-attach pilish--org-live-chat (current-buffer)))

(defun pilish-org-live-source ()
  "Visit persisted source at point, refusing provisional live sections."
  (interactive)
  (when (org-entry-get nil "PI_LIVE_ID" t)
    (user-error "Live section has no persisted entry ID yet; refresh while idle"))
  (unless (and pilish--org-view-path (file-readable-p pilish--org-view-path))
    (user-error "Session has no readable JSONL file yet"))
  (pilish-org-view-source))

(defun pilish-org-live-sparse-tree ()
  "Freeze this snapshot before invoking a native Org sparse tree.
The first slice does not replay arbitrary native sparse queries during streams."
  (interactive)
  (when pilish--org-live-process (pilish--org-live-flush))
  (pilish-org-live-detach)
  (call-interactively #'org-sparse-tree))

(defvar pilish-org-live-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'pilish-org-live-refresh)
    (define-key map (kbd "C-c C-r") #'pilish-org-live-refresh)
    (define-key map (kbd "C-c C-d") #'pilish-org-live-detach)
    (define-key map (kbd "s") #'pilish-org-live-source)
    (define-key map (kbd "C-c C-s") #'pilish-org-live-source)
    (define-key map (kbd "b") #'undefined)
    (define-key map (kbd "C-c C-b") #'undefined)
    (define-key map [remap org-sparse-tree] #'pilish-org-live-sparse-tree)
    map)
  "Keymap for the optional live Org companion.")

(define-derived-mode pilish-org-live-mode pilish-org-view-mode "Pi-Org-Live"
  "Read-only companion; normal interaction remains in Pilish's own buffers.
\\<pilish-org-live-mode-map>Use \\[pilish-org-live-detach] to freeze/detach and
\\[pilish-org-live-refresh] to reload raw history while idle.
\\[pilish-org-live-sparse-tree] freezes before filtering.
Direct sparse-query calls require detaching first."
  (add-hook 'kill-buffer-hook #'pilish-org-live-detach nil t)
  (add-hook 'change-major-mode-hook #'pilish-org-live-detach nil t))

;;;###autoload
(defun pilish-org-live ()
  "Open an optional live Org companion for the current idle Pilish session."
  (interactive)
  (let ((chat (pilish--get-chat-buffer)))
    (unless (and (buffer-live-p chat)
                 (process-live-p (buffer-local-value 'pilish--process chat)))
      (user-error "No running Pilish session"))
    (with-current-buffer chat
      (unless (and (eq pilish--status 'idle) (not (pilish--session-busy-p)))
        (user-error "Attach the Org companion while Pilish is idle")))
    (let ((buffer (generate-new-buffer (format "*Pi Org Live: %s*" (buffer-name chat)))))
      (condition-case err
          (progn
            (with-current-buffer buffer
              (let ((org-inhibit-startup t)) (pilish-org-live-mode)))
            (pilish--org-live-attach chat buffer)
            (pop-to-buffer buffer))
        ((error quit)
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (signal (car err) (cdr err)))))))

(add-hook 'pilish-event-observer-functions #'pilish--org-live-observe)
(add-hook 'pilish-exit-observer-functions #'pilish--org-live-exit)

(provide 'pilish-org-live)
;;; pilish-org-live.el ends here
