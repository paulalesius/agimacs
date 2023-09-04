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
        recentf-max-saved-items 100)

  ;; Misc tweaks
  (menu-bar-mode -1)
  (tool-bar-mode -1))

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
  :after (evil magit)
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

   ;; flycheck
   "cn" 'flycheck-next-error
   "cp" 'flycheck-previous-error
   "cl" 'flycheck-list-errors

   ;; help
   "hk" 'describe-key
   "hm" 'describe-mode
   "hv" 'describe-variable

   ;; buffer
   "bd" 'kill-current-buffer

   ;; projectile
   ;;"pp" 'projectile-switch-project
   )

  )

(use-package projectile
  :config
  (projectile-mode +1)
  (my-leader-def
   :keymaps 'projectile-mode-map
   "p" '(:ignore t :which-key "projectile")
   "p p" 'projectile-switch-project))

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

(use-package magit
  :commands magit-file-delete)

(use-package magit-todos
  :after magit
  :custom
  (magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?" "Allow TODO without colons TODO:"))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :after general
  :config
  (my-leader-def
   "b b" #'consult-buffer
   "f r" #'consult-recent-file)
  ;; Re-define standard keys
  (general-define-key
   :prefix "C-c"
   "b" #'consult-buffer))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  :init
  (setq lsp-clients-python-command "pylsp"
        lsp-enable-snippet nil
        lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-position 'at-point "Doesn't seem to work either. Childframes or WebKit frames require GUI widgets.")
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode))

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
  :after general
  :config
  (my-leader-def
   :keymaps 'python-mode-map
   "m" '(:ignore t :which-key "python")
   "m s" '(:ignore t :which-key "REPL")
   ;; REPL
   "m s r" '(python-shell-send-region :which-key "send region")
   "m s b" '(python-shell-send-buffer :which-key "send buffer")
   "m s f" '(python-shell-send-file :which-key "send file")))
;;:config
;; IPython REPL. I use a terminal mainly so there's no need for ipython(?)
;;(setq python-shell-interpreter "ipython"
;;      python-shell-interpreter-args "-i --simple-prompt"))

(use-package poetry
  :after python
  :custom
  (poetry-tracking-strategy 'switch-buffer)
  :hook
  (python-mode . #'poetry-tracking-mode))

(use-package python-pytest
  :after python
  :commands python-pytest-dispatch
  :config
  (my-leader-def
   :keymaps 'python-mode-map
   "m t" '(:ignore t :which-key "pytest")
   ;; Testing
   "m t a" #'python-pytest
   "m t f" #'python-pytest-file
   "m t F" #'python-pytest-function
   "m t r" #'python-pytest-repeat
   "m t d" #'python-pytest-dispatch))
