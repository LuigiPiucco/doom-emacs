;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-completion-styles '(basic partial-completion flex)
  "Completion styles for corfu to use.

If the user enables +orderless, `orderless' is automatically appended to this
list before fowarding to `completion-styles'.")

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
  :config
  (setq corfu-cycle t
        corfu-separator (when (modulep! +orderless) ?\s)
        corfu-preselect t
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (modulep! +orderless) 'separator t)
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))

  (when (modulep! +orderless)
    (after! lsp-mode
      (add-to-list 'completion-category-overrides
                   `(lsp-capf (styles ,@+corfu-completion-styles ,(when (modulep! +orderless) 'orderless)))))
    (after! eglot
      (add-to-list 'completion-category-overrides
                   `(eglot (styles ,@+corfu-completion-styles ,(when (modulep! +orderless) 'orderless))))))

  (after! evil
    (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

  (when (modulep! +icons)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  ;; This is to decouple the use of `completion-styles' in corfu from other
  ;; completion packages, such as vertico. That way, the user can leave the
  ;; global value of the variable alone, say, to be used by the default
  ;; front-end or consult. The vertico module also does something similar with
  ;; `+vertico-company-completion-styles'.
  (defadvice! +corfu--completion-styles (orig &rest args)
    "Try default completion styles before orderless.

Meant as :around advice for `corfu--recompute'."
    :around #'corfu--recompute
    (let ((completion-styles
           (append +corfu-completion-styles (when (modulep! +orderless)
                                              '(orderless))))
          completion-category-overrides completion-category-defaults)
      (apply orig args)))

  (map! (:unless (modulep! +tng)
          "C-SPC" #'completion-at-point)
        (:map 'corfu-map
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
    (map! :map 'corfu-map "M-m" #'corfu-move-to-minibuffer
          (:when (modulep! :editor evil) :i "M-j" #'corfu-move-to-minibuffer))))

(use-package! cape
  :defer t
  :init
  (add-hook! prog-mode (add-to-list 'completion-at-point-functions #'cape-file))
  (add-hook! (org-mode markdown-mode) (add-to-list 'completion-at-point-functions #'cape-elisp-block))
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package! yasnippet-capf
  :when (modulep! :editor snippets)
  :defer t
  :init
  (after! yasnippet
    (add-hook! yas-minor-mode
      (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

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
