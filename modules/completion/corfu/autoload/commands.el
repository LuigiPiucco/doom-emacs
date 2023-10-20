;;; completion/corfu/autoload/commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun corfu-move-to-minibuffer ()
  ;; Taken from corfu's README.
  ;; TODO: extend this to other completion front-ends.
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        (completion-cycle-threshold completion-cycling))
    (apply #'consult-completion-in-region completion-in-region--data)))

;;;###autoload
(defun corfu-insert-space-maybe-quit ()
  (interactive)
  (when (and (> (point) (point-min))
             (eq (char-before) +orderless-wildcard-character))
    (corfu-quit))
  (call-interactively
   (keymap-lookup
    (thread-last
      (current-active-maps t)
      (delq corfu-map)
      (delq (and (featurep 'evil)
                 (evil-get-auxiliary-keymap corfu-map
                                            evil-state))))
    "SPC")))
