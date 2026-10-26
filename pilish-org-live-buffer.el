;;; pilish-org-live-buffer.el --- Stable Org companion sections -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Buffer mechanics only: semantic headings stay put while accumulated content
;; is converted by the live adapter.  Never suppress Org modification hooks.

;;; Code:

(require 'cl-lib)
(require 'pilish-org-view)

(cl-defstruct (pilish--org-live-section
               (:constructor pilish--org-live-section-create))
  "A generated heading and its body, excluding any child sections."
  start body end)

(defun pilish--org-live-change (function)
  "Call FUNCTION with a writable, widened view, preserving reading state.
Only unrestricted windows whose points were at the tail follow new output."
  (let* ((restricted (buffer-narrowed-p))
         (minimum (copy-marker (point-min) t))
         (maximum (copy-marker (point-max)))
         (position (copy-marker (point)))
         (windows
          (mapcar (lambda (window)
                    (list window
                          (copy-marker (window-point window))
                          (copy-marker (window-start window))
                          (and (not restricted)
                               (>= (window-point window) (1- (point-max))))))
                  (get-buffer-window-list (current-buffer) nil t))))
    (unwind-protect
        (save-restriction
          (widen)
          (let ((inhibit-read-only t))
            (funcall function)))
      (set-buffer-modified-p nil)
      ;; Unlike save-restriction's growing end, a subtree restriction must not
      ;; absorb a newly appended sibling at its old end boundary.
      (when restricted
        (widen)
        (narrow-to-region minimum maximum))
      (set-marker minimum nil)
      (set-marker maximum nil)
      (goto-char (min (point-max) (max (point-min) position)))
      (dolist (state windows)
        (pcase-let ((`(,window ,point ,start ,follow) state))
          (when (window-live-p window)
            (set-window-start window start t)
            (set-window-point window (if follow (point-max) point)))
          (set-marker point nil)
          (set-marker start nil)))
      (set-marker position nil))))

(defun pilish--org-live-insert-section
    (position title entry level folded properties kind)
  "Insert a section at POSITION with TITLE, ENTRY and LEVEL.
FOLDED, PROPERTIES and KIND have the viewer heading helper's meanings.
Return its stable boundary markers.  Caller uses `pilish--org-live-change'."
  (goto-char position)
  (let ((start (point)) body end)
    (pilish--org-view-heading title entry level folded properties kind)
    (setq body (point))
    ;; Nonempty separators disambiguate neighboring marker boundaries.
    (insert "\n\n")
    (setq end (point))
    (save-excursion
      (goto-char start)
      (forward-line 1)
      (when (and org-cycle-hide-drawer-startup
                 (not (eq org-startup-folded 'showeverything))
                 (looking-at-p ":PROPERTIES:"))
        ;; Native drawer folds start at the opening line's END, leaving
        ;; :PROPERTIES: visible.  Hiding from its beginning creates a malformed
        ;; fold that Org later reveals from post-command-hook's fragility check.
        (org-fold-hide-drawer-toggle t))
      (goto-char start)
      (when folded (org-fold-hide-subtree)))
    (pilish--org-live-section-create
     :start (copy-marker start t)
     :body (copy-marker body)
     :end (copy-marker end))))

(defun pilish--org-live-update-body (section text)
  "Update SECTION's body to TEXT without replacing its semantic heading.
Use a bounded textual diff to retain unchanged text, markers and folds.
Caller uses `pilish--org-live-change'."
  (let* ((begin (pilish--org-live-section-body section))
         (end (pilish--org-live-section-end section))
         (text (concat (string-trim-right text) "\n\n")))
    (unless (equal text (buffer-substring-no-properties begin end))
      (let ((start (marker-position begin)))
        (replace-region-contents begin end (lambda () text) 0.02)
        (set-marker begin start)
        (set-marker end (+ start (length text)))))))

(defun pilish--org-live-section-properties (section properties)
  "Update SECTION's drawer from string alist PROPERTIES.
A nil value removes the property.  Caller uses `pilish--org-live-change'."
  (save-excursion
    (goto-char (pilish--org-live-section-start section))
    (dolist (property properties)
      (let ((value (and (cdr property) (pilish--org-view-one-line (cdr property)))))
        (unless (equal value (org-entry-get nil (car property)))
          (if value (org-entry-put nil (car property) value)
            (org-entry-delete nil (car property))))))))

(defun pilish--org-live-remove-section (section)
  "Remove SECTION and release its markers, excluding child sections.
Caller uses `pilish--org-live-change'."
  (delete-region (pilish--org-live-section-start section)
                 (pilish--org-live-section-end section))
  (pilish--org-live-release-section section))

(defun pilish--org-live-release-section (section)
  "Release SECTION's boundary markers without changing buffer text."
  (dolist (marker (list (pilish--org-live-section-start section)
                        (pilish--org-live-section-body section)
                        (pilish--org-live-section-end section)))
    (set-marker marker nil)))

(provide 'pilish-org-live-buffer)
;;; pilish-org-live-buffer.el ends here
