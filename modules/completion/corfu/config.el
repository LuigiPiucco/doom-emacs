;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +cape-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-line' or `cape-dabbrev'.")

(defvar +orderless-wildcard-character ?,
  "A character used as a wildcard in Corfu for fuzzy autocompletion. If you
want to match the wildcard literally in completion, you can
escape it with forward slash. Do NOT set this to SPC.

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

  (defun corfu-visible-p ()
    (or (and (frame-live-p corfu--frame)
             (frame-visible-p corfu--frame))
        (and (featurep 'corfu-terminal)
             (popon-live-p corfu-terminal--popon))))

  ;; If you want to update the visual hints after completing minibuffer commands
  ;; with Corfu and exiting, you have to do it manually.
  (defadvice! +corfu--insert-before-exit-minibuffer-a ()
    :before #'exit-minibuffer
    (when (corfu-visible-p)
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        ;; Pending a PR I am making to expose `anzu--update-timer'.
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer)))))

  (defadvice! +corfu--return-insert-a (oldfun &rest args)
    "Do not make us type RET twice with Corfu."
    :around #'corfu-insert
    (let ((index corfu--index))
      (apply oldfun args)
      (when (and (member (this-command-keys-vector)
                         (list (vector 'return) (vector ?\r)))
                 (eq (point) (point-at-eol))
                 ;; Only do this if we did not select a completion.
                 (eq index -1))
        (call-interactively
         (keymap-lookup
          (thread-last (current-active-maps t)
                       (delq corfu-map)
                       (delq (and (featurep 'evil)
                                  (evil-get-auxiliary-keymap corfu-map
                                                             evil-state))))
          "RET")))))

  ;; Allow completion after `:' in Lispy.
  (add-to-list 'corfu-auto-commands #'lispy-colon)

  (when (and (modulep! +orderless)
             +orderless-wildcard-character)
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
                  #'corfu-insert-separator)
      ;; Quit completion after typing the wildcard followed by a space.
      (keymap-set corfu-map "SPC"
                  (cmd! ()
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
                          "SPC"))))

      ;; Require that for Corfu, candidates begin with the first component of
      ;; the prefix. I added this because it was jarring to use `cape-dict' but
      ;; see many words that did not begin with the prefix.
      (add-hook! 'orderless-style-dispatchers
        (defun +corfu-orderless-dispatch (pattern index total)
          (when (and completion-in-region--data (eq index 0))
            (cons 'orderless-regexp (concat "^" (regexp-quote pattern))))))))

  (add-hook! 'evil-insert-state-exit-hook
    (defun +corfu-quit-on-evil-insert-state-exit-h ()
      ;; This is a workaround for unexpected calls to `corfu-quit' in
      ;; :company-doc-buffer buffers. This was specifically happening when using
      ;; `yasnippet-capf' and `company-yasnippet'.
      (when (eq (current-buffer) (window-buffer (selected-window)))
        (corfu-quit))))


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
        dabbrev-upcase-means-case-search t
        dabbrev-case-fold-search nil)   ; Reduce garbage `cape-dabbrev' results.

  (add-hook! prog-mode
    (add-hook 'completion-at-point-functions #'cape-file -10 t))
  (add-hook! (org-mode markdown-mode)
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (add-hook! (prog-mode text-mode conf-mode comint-mode minibuffer-setup)
    (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))

  ;; Complete emojis :).
  (when (> emacs-major-version 28)
    (add-hook! (prog-mode conf-mode)
      (add-hook 'completion-at-point-functions
                (cape-capf-inside-docstring-or-comment
                 (cape-capf-prefix-length #'cape-emoji 1))
                10 t))
    (add-hook! text-mode
      (add-hook 'completion-at-point-functions
                (cape-capf-prefix-length #'cape-emoji 1) 10 t)))

  ;; Enable dictionary-based autocompletion.
  (add-hook! text-mode
    (add-hook 'completion-at-point-functions #'cape-dict 40 t))
  (add-hook! (prog-mode conf-mode)
    (add-hook 'completion-at-point-functions
              (cape-capf-inside-docstring-or-comment #'cape-dict)
              40 t))

  ;; Make these capfs composable.
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)

  ;; From the `cape' readme. Without this, Eshell autocompletion is broken on
  ;; Emacs28.
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package! yasnippet-capf
  :when (modulep! :editor snippets)
  :defer t
  :init
  (add-hook! yas-minor-mode
    (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t)))

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
