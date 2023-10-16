;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +cape-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-line' or `cape-dabbrev'.")

(defvar +orderless-wildcard-character ?,
  "A character used as a wildcard in Corfu for fuzzy autocompletion. If you
want to match the wildcard literally in completion, you can
escape it with forward slash.

This variable needs to be set at the top-level before any `after!' blocks.")

;;
;;; Packages
(use-package! corfu
  :hook (doom-first-buffer . global-corfu-mode)
  :hook (org-mode . corfu-mode)
  :init
  ;; Auto-completion settings, must be set before calling `global-corfu-mode'.
  ;; Due to lazy-loading, overriding these in config.el works too.
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               help-mode
                               gud-mode
                               vterm-mode))
  (map! (:unless (modulep! +tng)
         :i "C-SPC" #'completion-at-point))
  :config
  (setq corfu-cycle t
        corfu-preselect (if (modulep! :completion corfu +tng) 'prompt t)
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (modulep! +orderless) 'separator t)
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))

  (defun corfu-disable-in-minibuffer-p ()
    (or (bound-and-true-p mct--active)
        (bound-and-true-p vertico--input)
        (and (featurep 'helm-core) (helm--alive-p))
        (eq (current-local-map) read-passwd-map)))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (unless (corfu-disable-in-minibuffer-p)
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (defadvice! +corfu--submit-candidate-to-shell-a (fn &rest args)
    "Make `corfu' behave in `comint' as it does in the minibuffer.

Do not allow `RET' for autocompletion in Comint & Eshell buffers.
This provides a middle ground between the `TAB' only completion
typical of shells and the full autocompletion of Corfu."
    :around #'corfu-insert
    (if (and (or (derived-mode-p 'eshell-mode)
                 (derived-mode-p 'comint-mode))
             corfu-auto)
        (call-interactively
         (keymap-lookup (symbol-value
                         (intern (concat (symbol-name major-mode) "-map")))
                        "RET"))
      (apply fn args)))

  ;; Allow completion after `:' in Lispy.
  (add-to-list 'corfu-auto-commands #'lispy-colon)

  (when (modulep! +orderless)
    (defmacro +orderless-escapable-split-fn (char)
      (let ((char-string (string (if (symbolp char) (symbol-value char) char))))
        `(defun +orderless-escapable-split-on-space-or-char (s)
           (mapcar
            (lambda (piece)
              (replace-regexp-in-string
               (string 1) ,char-string
               (replace-regexp-in-string
                (concat (string 0) "\\|" (string 1))
                (lambda (x)
                  (pcase x
                    ("\0" " ")
                    ("\1" ,char-string)
                    (_ x)))
                piece
                ;; These are arguments to `replace-regexp-in-string'.
                'fixedcase 'literal)
               'fixedcase 'literal))
            (split-string (replace-regexp-in-string
                           (concat "\\\\\\\\\\|\\\\ \\|\\\\"
                                   ,char-string)
                           (lambda (x)
                             (pcase x
                               ("\\ " "\0")
                               (,(concat "\\" char-string)
                                "\1")
                               (_ x)))
                           s 'fixedcase 'literal)
                          ,(concat "[ " char-string "]+")
                          ;; If we want some fancy logic in the future for
                          ;; ",PREFIX", we will have to keep nulls but for now,
                          ;; remove them.
                          t)))))
    (after! orderless
      ;; Orderless splits the string into components and then determines the
      ;; matching style for each component. This is all regexp stuff.
      (setq orderless-component-separator
            (+orderless-escapable-split-fn +orderless-wildcard-character))
      (setq corfu-separator +orderless-wildcard-character)
      (keymap-set corfu-map (char-to-string +orderless-wildcard-character)
                  #'corfu-insert-separator)))

  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  (when (modulep! +icons)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (map! (:map 'corfu-map
         (:when (modulep! +orderless)
          "C-SPC" #'corfu-insert-separator)
         (:when (modulep! +tng)
          [tab] #'corfu-next
          [backtab] #'corfu-previous
          "TAB" #'corfu-next
          "S-TAB" #'corfu-previous)))
  (after! evil-collection-corfu
    (evil-collection-define-key 'insert 'corfu-map
      (kbd "RET") #'corfu-insert
      [return] #'corfu-insert))

  (after! vertico
    ;; Taken from corfu's README.
    ;; TODO: extend this to other completion front-ends.
    (defun corfu-move-to-minibuffer ()
      (interactive)
      (let ((completion-extra-properties corfu--extra)
            (completion-cycle-threshold completion-cycling))
        (apply #'consult-completion-in-region completion-in-region--data)))
    (map! :map 'corfu-map "M-m" #'corfu-move-to-minibuffer)
    (after! evil-collection-corfu
      (evil-collection-define-key 'insert 'corfu-map
        (kbd "M-j") #'corfu-move-to-minibuffer))))

(use-package! cape
  :defer t
  :init
  ;; Set up `cape-dabbrev' and `cape-line' options.
  (defun +cape-line-buffers ()
    (cl-loop for buf in (buffer-list)
             if (or (eq major-mode (buffer-local-value 'major-mode buf))
                    (< (buffer-size buf) +cape-buffer-scanning-size-limit))
             collect buf))
  (defun +dabbrev-friend-buffer-p (other-buffer)
    (< (buffer-size other-buffer +cape-buffer-scanning-size-limit)))
  (setq cape-dabbrev-check-other-buffers t
        cape-line-buffer-function #'+cape-line-buffers
        dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
        dabbrev-upcase-means-case-search t)

  (add-hook! prog-mode
    (add-hook 'completion-at-point-functions #'cape-file -1 t))
  (add-hook! (org-mode markdown-mode)
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  ;; Complete emojis :).
  (when (> emacs-major-version 28)
    (add-hook! (prog-mode conf-mode)
      (add-hook 'completion-at-point-functions
                (cape-capf-inside-comment
                 (cape-capf-prefix-length #'cape-emoji 1))
                90 t))
    (add-hook! text-mode
      (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-emoji 1) 90 t)))

  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (add-hook! (text-mode conf-mode comint-mode)
    (add-hook 'completion-at-point-functions #'cape-dabbrev 91 t))

  ;; Enable dictionary-based autocompletion.
  (add-hook! text-mode
    (add-hook 'completion-at-point-functions #'cape-dict 93 t))

  ;; Set the autocompletion backends to enable for the minibuffer.
  (add-hook! 'minibuffer-setup-hook
    (add-hook 'completion-at-point-functions #'cape-dabbrev 91 t))

  ;; Make these capfs composable.
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)

  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package! yasnippet-capf
  :when (modulep! :editor snippets)
  :defer t
  :init
  (add-hook! yas-minor-mode
    (add-hook 'completion-at-point-functions #'yasnippet-capf 92 t)))

(use-package! corfu-terminal
  :when (not (display-graphic-p))
  :hook (corfu-mode . corfu-terminal-mode))

;;
;;; Extensions

(use-package! corfu-history
  :hook (corfu-mode . corfu-history-mode)
  :config
  (after! savehist (add-to-list 'savehist-additional-variables 'corfu-history)))


(use-package! corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0))
  (map! (:map 'corfu-map
              "C-<up>" #'corfu-popupinfo-scroll-down
              "C-<down>" #'corfu-popupinfo-scroll-up
              "C-S-p" #'corfu-popupinfo-scroll-down
              "C-S-n" #'corfu-popupinfo-scroll-up
              "C-h" #'corfu-popupinfo-toggle)
        (:map 'corfu-popupinfo-map
         :when (modulep! :editor evil)
         ;; Reversed because popupinfo assumes opposite of what feels intuitive
         ;; with evil.
         "C-S-k" #'corfu-popupinfo-scroll-down
         "C-S-j" #'corfu-popupinfo-scroll-up)))
