;;; completion/corfu/autoload/commands.el -*- lexical-binding: t; -*-

(defun +corfu--get-passthrough-command ()
  (keymap-lookup
   (thread-last
     (current-active-maps t)
     (delq corfu-map)
     (delq (and (featurep 'evil)
                (evil-get-auxiliary-keymap corfu-map evil-state))))
   (key-description (this-command-keys-vector))))

;;;###autoload
(defun corfu-move-to-minibuffer ()
  ;; Taken from corfu's README.
  ;; TODO: extend this to other completion front-ends.
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        (completion-cycle-threshold completion-cycling))
    (apply #'consult-completion-in-region completion-in-region--data)))

;;;###autoload
(defun +corfu-insert-space-maybe-quit ()
  ;; I had to rename this command so that it doesn't start with "corfu-".
  ;; Otherwise, it does not insert the completion when +tng is enabled.
  (interactive)
  (when (and (> (point) (point-min))
             (eq (char-before) +orderless-wildcard-character))
    (corfu-quit))
  (let ((command (+corfu--get-passthrough-command)))
    (setq this-command command)
    (call-interactively command)))

;;;###autoload
(defun +corfu-insert-wildcard-separator ()
  ;; I had to rename this command so that it doesn't start with "corfu-".
  ;; Otherwise, it does not insert the completion when +tng is enabled.
  (interactive)
  (setq this-command #'corfu-insert-separator)
  (call-interactively #'corfu-insert-separator))

;;;###autoload
(defun corfu-reset-completion-or-backspace ()
  (interactive)
  (let ((command (+corfu--get-passthrough-command)))
    (if (and (> corfu--index -1)
             (eq corfu-preview-current 'insert))
        (progn
          (setq this-command #'corfu-reset)
          (call-interactively #'corfu-reset))
      (setq this-command command)
      (call-interactively command))))
