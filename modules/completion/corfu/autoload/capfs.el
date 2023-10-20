;;; completion/corfu/autoload/capfs.el -*- lexical-binding: t; -*-

(defun doom-point-in-docstring-or-comment-p ()
  "Check if the point is in a docstring or comment."
  (let ((faces (get-text-property (point) 'face)))
    (pcase faces
      ((or 'tree-sitter-hl-face:doc 'font-lock-doc-face) t)
      ((pred listp) (or (memq 'tree-sitter-hl-face:doc faces)
                        (memq 'font-lock-doc-face faces)))
      (_ (doom-point-in-comment-p)))))

(defun cape-wrap-inside-docstring-or-comment (capf)
  (and (doom-point-in-docstring-or-comment-p) (funcall capf)))

(require 'cape)

;;;###autoload (autoload 'cape-capf-inside-docstring-or-comment "completion/corfu/autoload")
(cape--capf-wrapper inside-docstring-or-comment)
