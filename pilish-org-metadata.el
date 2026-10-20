;;; pilish-org-metadata.el --- Org transcript classification and graph metadata -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Keep session-tree facts separate from Org outline structure.  Child counts
;; describe canonical JSONL entries across all branches, not rendered headings.
;; Thinking settings are replayed only along the selected ancestor path.

;;; Code:

(require 'pilish-jsonl)
(require 'seq)
(require 'subr-x)

(defun pilish--org-view-string (value)
  "Return VALUE when it is a nonempty string, otherwise nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun pilish--org-view-kind (entry)
  "Return the semantic Org kind of source ENTRY."
  (pcase (plist-get entry :type)
    ("message"
     (pcase (plist-get (plist-get entry :message) :role)
       ("toolResult" "tool_result")
       ("bashExecution" "shell")
       ((or "compactionSummary" "branchSummary") "summary")
       ((and role (or "user" "assistant" "system" "custom")) role)
       (_ "message")))
    ((or "compaction" "branch_summary") "summary")
    ("custom_message" "custom")
    ("usage" "usage")
    (_ "event")))

(defun pilish--org-view-kind-tags (kind &optional tool-name)
  "Return native Org tags for KIND and optional TOOL-NAME.
Only user prompts and response bodies get pi_prose.  In particular the
assistant envelope must not give that tag to thinking/tool descendants."
  (append (list (concat "pi_" kind))
          (when (member kind '("user" "response")) '("pi_prose"))
          (when (member kind '("tool_result" "shell")) '("pi_tool"))
          (when (pilish--org-view-string tool-name)
            (list (concat "pi_tool_"
                          (replace-regexp-in-string
                           "[^[:alnum:]_]" "_" (downcase tool-name)))))))

(defun pilish--org-view-build-metadata (session branch)
  "Build a source-entry context table for SESSION and selected BRANCH.
The eq-keyed table stores :properties and :tags for original entry objects.
Graph facts come from Pilish's canonical tree, including off-path children.
No source entries are mutated.  Unknown thinking levels remain absent."
  (let* ((tree (pilish-jsonl-build-tree (plist-get session :entries)))
         (stack (mapcar (lambda (node) (list node nil 0))
                        (append (plist-get tree :tree) nil)))
         (metadata (make-hash-table :test #'eq)))
    (while stack
      (pcase-let* ((`(,node ,parent ,siblings) (pop stack))
                   (entry (plist-get node :entry))
                   (children (plist-get node :children))
                   (count (length children))
                   (properties (when (pilish--org-view-string (plist-get entry :id))
                                 (list (cons "PI_CHILD_COUNT" count))))
                   (tags nil))
        (when (> count 1) (push "pi_branchpoint" tags))
        (when (> siblings 1)
          (push "pi_branch_start" tags)
          (push (cons "PI_BRANCH_FROM_ID" (plist-get parent :id)) properties))
        (puthash entry (list :properties properties :tags tags) metadata)
        (seq-doseq (child children)
          (push (list child entry count) stack))))
    (let ((entries (plist-get branch :entries)) thinking)
      (while entries
        (let* ((entry (pop entries))
               (context (or (gethash entry metadata) (list :properties nil)))
               (properties (plist-get context :properties))
               (message (plist-get entry :message)))
          (when (equal (plist-get entry :type) "thinking_level_change")
            (setq thinking (pilish--org-view-string (plist-get entry :thinkingLevel))))
          (when (equal (plist-get message :role) "assistant")
            (when-let* ((level (or (pilish--org-view-string (plist-get message :thinkingLevel))
                                  thinking)))
              (push (cons "PI_THINKING_LEVEL" level) properties)))
          (when (and entries (member "pi_branchpoint" (plist-get context :tags)))
            (push (cons "PI_SELECTED_CHILD_ID" (plist-get (car entries) :id)) properties))
          (puthash entry (plist-put context :properties properties) metadata))))
    metadata))

(provide 'pilish-org-metadata)
;;; pilish-org-metadata.el ends here
