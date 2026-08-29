;;; pilish-org.el --- Org input support for pilish -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pilish

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Optional Org major-mode support for pilish input buffers.
;; The selected mode remains the buffer's real major mode; pi-specific input
;; behavior is supplied by a small minor mode instead.

;;; Code:

(require 'org)
(require 'org-element)
(require 'pilish-ui)

(defcustom pilish-input-major-mode nil
  "Major mode used to compose prompts.

When nil, use pilish's default input mode with Markdown
highlighting controlled by `pilish-input-markdown-highlighting'.
Set this to `org-mode' to compose prompts in Org.  The selected mode is the
input buffer's actual major mode, so its commands and fontification work
normally.  pilish's default input bindings are not installed in Org
input buffers; add preferred bindings to `pilish-input-mode-map'.

Takes effect for new input buffers; existing input buffers keep their mode."
  :type '(choice (const :tag "Pi Input (Markdown highlighting)" nil)
                 (const :tag "Org" org-mode))
  :group 'pilish)

(defcustom pilish-org-ignore-elements '(property-drawer)
  "Org element types removed from prompts before sending.

Like gptel, pilish removes property drawers by default.  The input
buffer and its history retain the original text; only the prompt sent to pi is
filtered.  Adding element types can slow prompt submission because Org must
parse the input buffer."
  :type '(repeat symbol)
  :group 'pilish)

(defvar pilish-org-input-mode-hook nil
  "Hook run after Org is initialized as a pi input buffer.")

(define-minor-mode pilish-org-input-mode
  "Minor mode providing pi input bindings in an Org input buffer."
  :init-value nil
  :lighter nil
  :keymap pilish-input-mode-map)

(defun pilish-org--input-mode-p ()
  "Return non-nil when the configured input mode is Org."
  (eq pilish-input-major-mode 'org-mode))

(defun pilish-org--setup-input-buffer ()
  "Make the current buffer an Org-mode pi input buffer."
  (org-mode)
  (pilish-org-input-mode 1)
  (run-hooks 'pilish-org-input-mode-hook))

(defun pilish-org--preprocess-input (text)
  "Return TEXT with ignored Org elements removed.

This follows gptel's default prompt preprocessing: property drawers are
removed before the prompt is sent.  TEXT itself is not modified."
  (if (null pilish-org-ignore-elements)
      text
    (with-temp-buffer
      (insert text)
      (org-mode)
      (let (bounds)
        (org-element-map (org-element-parse-buffer 'element)
            pilish-org-ignore-elements
          (lambda (element)
            (push (cons (org-element-property :begin element)
                        (org-element-property :end element))
                  bounds)))
        ;; Delete from the end so positions collected from the parse tree stay
        ;; valid even when users choose nested element types.
        (dolist (bound (sort bounds (lambda (a b) (> (car a) (car b)))))
          (delete-region (car bound) (cdr bound)))
        (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'pilish-org)

;;; pilish-org.el ends here
