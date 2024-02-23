;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-complete-and-exit-minibuffer ()
  (interactive)
  (if (>= corfu--index 0)
      (corfu-complete)
    (corfu-insert))
  (exit-minibuffer))

;;;###autoload
(defun +corfu-move-to-minibuffer ()
  "Move the current list of candidates to your choice of minibuffer completion UI."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (cond ((and (modulep! :completion vertico)
                   (fboundp #'consult-completion-in-region))
              (consult-completion-in-region beg end table pred))
             ((and (modulep! :completion ivy)
                   (fboundp #'ivy-completion-in-region))
              (ivy-completion-in-region (marker-position beg) (marker-position end) table pred))
             ;; Helm is special and wants to _wrap_ `completion--in-region'
             ;; instead of replacing it in `completion-in-region-function'.
             ((and (modulep! :completion helm)
                   (fboundp #'helm--completion-in-region)
                   (advice-member-p #'helm--completion-in-region #'completion--in-region))
              ;; Important: `completion-in-region-function' is set to corfu at
              ;; this moment, so `completion-in-region' (single -) doesn't work.
              (completion--in-region beg end table pred))
             ;; Ido doesn't implement `completion-in-region', and its
             ;; `completing-read' only accepts a plain list of strings as table,
             ;; so there's not much we can do with it.
             (t (error "No minibuffer completion UI available for moving to!")))))))

;;;###autoload
(defun +corfu-smart-sep-toggle-escape ()
  "Insert `corfu-separator' or toggle escape if it's already there."
  (interactive)
  (cond ((and (char-equal (char-before) corfu-separator)
              (char-equal (char-before (1- (point))) ?\\))
         (save-excursion (delete-char -2)))
        ((char-equal (char-before) corfu-separator)
         (save-excursion (backward-char 1)
                         (insert-char ?\\)))
        (t
         ;; Without this corfu quits immediately.
         (setq this-command #'corfu-insert-separator)
         (call-interactively #'corfu-insert-separator))))

;;;###autoload
(defun +corfu-in-doc-or-comment-p (_sym)
  "Return non-nil if point is in a docstring or comment."
  (or (nth 4 (syntax-ppss))
      (when-let ((faces '(font-lock-comment-face
                          font-lock-doc-face
                          tree-sitter-hl-face:doc
                          tree-sitter-hl-face:comment))
                 (fs (get-text-property (point) 'face)))
        (if (listp fs)
            (cl-loop for f in fs
                     if (memq f faces)
                     return t)
          (memq fs faces)))))
