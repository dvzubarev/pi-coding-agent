;;; pilish-org-markdown.el --- Prisma-based Markdown to Org -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Block/list traversal and rendering adapted from prisma-md.el and
;; prisma-org.el, Copyright (C) 2026 Ag Ibragimov (GPL-3.0-or-later).
;; Tested with Prisma 0bcb4ff696eac9832a0d26d48093e4ce35e1145b.

;;; Commentary:

;; Optional converter for the offline session viewer.  Reuses Prisma's model,
;; inline parser and table parser, not its mirror UI or public conversion API.
;; Local traversal retains nested list blocks and unknown block syntax.  Local
;; rendering offsets headings and protects Org source/example block boundaries.
;; No global advice or changes to Prisma are installed.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'prisma-md)

(defun pilish--org-literal-block (text &optional language headers)
  "Render TEXT as a safe example block, or source block in LANGUAGE.
HEADERS is an optional single-line string of trusted metadata parameters.
The final :eval never always follows metadata.  Never pass raw tool values
as HEADERS; serialize them with `pilish--org-tool-headers' first."
  (let* ((language (and language
                        (string-match-p "\\`[[:alnum:]_+.-]+\\'" language)
                        language))
         (kind (if language "src" "example"))
         (body (org-escape-code-in-string text)))
    (concat "#+begin_" kind
            (if language
                (concat " " language
                        (when (and headers (not (string-empty-p headers)))
                          (concat " " headers))
                        " :eval never") "")
            "\n" body (unless (string-suffix-p "\n" body) "\n")
            "#+end_" kind)))

(defun pilish--org-md-block-children (node inline-root text)
  "Convert block children of NODE using Prisma INLINE-ROOT and TEXT."
  (delq nil
        (mapcar (lambda (child)
                  (unless (string-match-p
                           "\\`\\(?:list_marker_.*\\|task_list_marker_.*\\|block_continuation\\|block_quote_marker\\)\\'"
                           (treesit-node-type child))
                    (pilish--org-md-block child inline-root text)))
                (treesit-node-children node t))))

(defun pilish--org-md-block (node inline-root text)
  "Convert tree-sitter NODE to Prisma's model using INLINE-ROOT and TEXT.
Unlike Prisma's block whitelist, unknown blocks are retained literally."
  (let ((type (treesit-node-type node))
        (start (treesit-node-start node))
        (end (treesit-node-end node)))
    (pcase type
      ((or "document" "section")
       (prisma-model-document
        :children (pilish--org-md-block-children node inline-root text)))
      ("atx_heading" (prisma-md--process-heading node inline-root text))
      ("setext_heading"
       (let* ((para (prisma-ts-child-by-type node "paragraph"))
              (level (if (prisma-ts-child-by-type node "setext_h1_underline")
                         1 2)))
         (prisma-model-heading
          :level level :children (and para
                                      (prisma-md--inline-children
                                       para inline-root text)))))
      ("paragraph" (prisma-md--process-paragraph node inline-root text))
      ("list"
       (let ((first (prisma-ts-child-by-type node "list_item")))
         (prisma-model-list
          :ordered (and first
                        (or (prisma-ts-child-by-type first "list_marker_dot")
                            (prisma-ts-child-by-type first "list_marker_parenthesis")))
          :children (pilish--org-md-block-children node inline-root text))))
      ("list_item"
       (prisma-model-list-item
        :checkbox (prisma-md--list-item-checkbox node)
        :children (pilish--org-md-block-children node inline-root text)))
      ("fenced_code_block"
       ;; Do not strip backticks from the body: they may be real code.
       (let* ((info (prisma-ts-child-by-type node "info_string"))
              (lang (and info (prisma-ts-child-by-type info "language")))
              (body (prisma-ts-child-by-type node "code_fence_content")))
         (prisma-model-code-block
          :language (and lang (treesit-node-text lang t))
          :body (if body (treesit-node-text body t) ""))))
      ("indented_code_block"
       (prisma-model-code-block
        :body (replace-regexp-in-string
               "^\\(?:    \\|\t\\)" "" (treesit-node-text node t))))
      ("block_quote"
       (prisma-model-blockquote
        :children (pilish--org-md-block-children node inline-root text)))
      ("pipe_table" (prisma-md--process-table node inline-root text))
      ("thematic_break" (prisma-model-horiz-rule))
      (_ (prisma-model-node 'literal-block
                            :props (list :text (substring text start end)))))))

(defun pilish--org-md-parse-buffer ()
  "Parse the current Markdown buffer into Prisma's document model.
Tree-sitter positions are buffer positions; TEXT is NUL-prefixed as in
Prisma's internal parser.  Keeping this seam buffer-based allows later
reuse with persistent parsers for streaming."
  (prisma-md--ensure-grammars)
  (let* ((block (treesit-parser-create 'markdown))
         (inline (treesit-parser-create 'markdown-inline)))
    (pilish--org-md-block
     (treesit-parser-root-node block)
     (treesit-parser-root-node inline)
     (concat "\0" (buffer-substring-no-properties (point-min) (point-max))))))

(defun pilish--org-md-render-children (node base)
  "Render inline children of model NODE beneath heading level BASE."
  (mapconcat (lambda (child) (pilish--org-md-render child base))
             (prisma-model-children node) ""))

(defun pilish--org-md-render-blocks (nodes base)
  "Render block NODES beneath heading level BASE."
  (prisma-model-render-blocks
   nodes (lambda (node) (pilish--org-md-render node base))))

(defun pilish--org-md-render-list-item (node base marker)
  "Render list item NODE beneath BASE with MARKER, retaining all blocks."
  (let* ((body (pilish--org-md-render-blocks
                (prisma-model-children node) base))
         (checkbox (pcase (prisma-model-prop node :checkbox)
                     ('checked "[X] ") ('unchecked "[ ] ") (_ "")))
         (lines (split-string body "\n"))
         (indent (make-string (length marker) ?\s)))
    (concat marker checkbox (or (car lines) "")
            (when (cdr lines)
              (concat "\n" (mapconcat
                            (lambda (line)
                              (if (string-empty-p line) "" (concat indent line)))
                            (cdr lines) "\n"))))))

(defun pilish--org-md-render-table (node base)
  "Render model table NODE beneath BASE, escaping cell separators."
  (let ((columns (apply #'max 1
                        (mapcar (lambda (row)
                                  (length (prisma-model-children row)))
                                (prisma-model-children node)))))
    (mapconcat
     (lambda (row)
       (if (eq (prisma-model-type row) 'table-separator)
           (concat "|" (mapconcat #'identity (make-list columns "---") "+") "|")
         (concat "| "
                 (mapconcat
                  (lambda (cell)
                    (replace-regexp-in-string
                     "|" "\\vert{}"
                     (replace-regexp-in-string
                      "\\\\|" "|"
                      (replace-regexp-in-string
                       "\n" " " (pilish--org-md-render-children cell base)) t t) t t))
                  (prisma-model-children row) " | ")
                 " |")))
     (prisma-model-children node) "\n")))

(defun pilish--org-md-render (node base)
  "Render Prisma model NODE as Org beneath heading level BASE."
  (pcase (prisma-model-type node)
    ('document (pilish--org-md-render-blocks (prisma-model-children node) base))
    ('heading
     (concat (make-string (+ base (prisma-model-prop node :level)) ?*)
             " " (pilish--org-md-render-children node base)))
    ('paragraph
     ;; Neutralize Org keywords/drawers originating in plain Markdown text.
     (replace-regexp-in-string
      "^\\([ \t]*\\)\\(#\\+\\|:[[:alnum:]_]+:[ \t]*$\\)"
      "\\1​\\2" (pilish--org-md-render-children node base)))
    ('text (prisma-model-prop node :value))
    ((or 'strong 'emphasis 'strike)
     (let ((mark (pcase (prisma-model-type node)
                   ('strong "*") ('emphasis "/") (_ "+"))))
       (concat mark (pilish--org-md-render-children node base) mark)))
    ((or 'code 'verbatim)
     (let* ((value (prisma-model-prop node :value))
            (mark (if (string-search "~" value) "=" "~")))
       (concat mark value mark)))
    ('link
     (format "[[%s][%s]]" (or (prisma-model-prop node :url) "")
             (pilish--org-md-render-children node base)))
    ('image (format "[[%s]]" (or (prisma-model-prop node :url) "")))
    ('linebreak "\\\\\n")
    ('code-block
     (pilish--org-literal-block (or (prisma-model-prop node :body) "")
                               (prisma-model-prop node :language)))
    ('list
     (let ((number 0))
       (mapconcat
        (lambda (item)
          (pilish--org-md-render-list-item
           item base (if (prisma-model-prop node :ordered)
                         (format "%d. " (cl-incf number)) "- ")))
        (prisma-model-children node) "\n")))
    ('blockquote
     (concat "#+begin_quote\n"
             (pilish--org-md-render-blocks (prisma-model-children node) base)
             "\n#+end_quote"))
    ('table (pilish--org-md-render-table node base))
    ('horiz-rule "-----")
    ('literal-block
     (pilish--org-literal-block (or (prisma-model-prop node :text) "")))
    (_ (or (prisma-model-prop node :text) ""))))

(defun pilish-org-markdown-to-org (text &optional base-level)
  "Convert Markdown TEXT to Org beneath BASE-LEVEL (default zero).
Uses pinned Prisma internals without modifying Prisma globally.
Unknown block syntax is retained in example blocks."
  (with-temp-buffer
    (insert text)
    (pilish--org-md-render (pilish--org-md-parse-buffer) (or base-level 0))))

(provide 'pilish-org-markdown)
;;; pilish-org-markdown.el ends here
