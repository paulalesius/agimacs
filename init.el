(straight-use-package 'use-package)

(use-package emacs
  :init
  (savehist-mode)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Enable recentf mode
  (recentf-mode 1)
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 100))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :custom
  (evil-collection-setup-minibuffer t)
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil
  :config
  (general-create-definer my-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (my-leader-def
   "ff" 'find-file
   "fr" 'recentf-open-files

    ;; flycheck
   "cn" 'flycheck-next-error
   "cp" 'flycheck-previous-error
   "cl" 'flycheck-list-errors))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package doom-modeline
  :init
  (setq doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :config
  :hook (after-init . doom-modeline-mode))

(use-package which-key
  :after evil
  :init
  (setq which-key-idle-delay 0.4)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  :init
  (setq lsp-clients-python-command "pylsp"
        lsp-enable-snippet nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t))

(use-package company-lsp
  :commands company-lsp)

(use-package flycheck
  :after lsp-mode
  ;;:hook (lsp-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
        flycheck-idle-change-delay 0.8)
  :config
  (global-flycheck-mode t))

(use-package python
  :ensure nil)
  ;;:config
  ;; IPython REPL. I use a terminal mainly so there's no need for ipython(?)
  ;;(setq python-shell-interpreter "ipython"
  ;;      python-shell-interpreter-args "-i --simple-prompt"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((projectile-globally-ignored-directories "dataset")
     (project-watch-ignored-directories "dataset"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
