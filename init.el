(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
      read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      recentf-max-menu-items 100
      recentf-max-saved-items 100)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(savehist-mode)
(recentf-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; @TODO: hack because of how the emacs daemon is run
(setenv "WAYLAND_DISPLAY" "wayland-1")
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
				      :buffer nil
				      :command '("wl-copy" "--primary" "-n")
				      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste --primary -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(defun toggle-line-number-mode ()
  "Toggle between relative and absolute line numbers."
  (interactive)
  (if (eq display-line-numbers-type 'relative)
      (setq display-line-numbers-type 'absolute)
    (setq display-line-numbers-type 'relative))
  (if display-line-numbers
      (display-line-numbers-mode 'toggle)
    (display-line-numbers-mode 1)))

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
  (evil-mode 1)
  (defun my-evil-visual-update-x-selection (orig-fun &rest args)
    (when interprogram-cut-function
      (funcall interprogram-cut-function
     	       (if (region-active-p)
     		   (buffer-substring-no-properties (region-beginning) (region-end))
     		 (car args))))
    (apply orig-fun args))
  (advice-add 'evil-visual-update-x-selection :around #'my-evil-visual-update-x-selection))

(use-package evil-collection
  :after (evil magit)
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package general
  :after evil)

(use-package projectile
  :after general
  :config
  (projectile-mode +1))

(use-package doom-modeline
  :custom
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes nil)
  :hook (after-init . doom-modeline-mode))

(use-package which-key
  :after evil
  :init
  (setq which-key-idle-delay 0.4)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package magit
  :after evil)

(use-package magit-todos
  :after magit
  :custom
  (magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?" "Allow TODO without colons TODO:"))

(use-package git-gutter
  :if (not (display-graphic-p))
  :after magit
  :custom
  (git-gutter:window-width 1)
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:modified-sign "=")
  :config
  (global-git-gutter-mode +1))

(use-package dashboard
  :custom
  (org-agenda-files '("/storage/src/unnsvc/org/general.org"))
  (dashboard-startup-banner (expand-file-name "dashboard.txt" user-emacs-directory))
  :config
  ;;(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package lookup
  :straight (lookup :type git :host github :repo "aaronjensen/emacs-lookup" :commit "6ffdb61ef7c70077dee45330d4444a0eec559e01")
  :after general)

(use-package helpful
  :after general)

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

  (use-package consult
    :after general

    )

(use-package consult-flycheck
  :after (consult flycheck))

(add-to-list 'straight-built-in-pseudo-packages 'keybindings)
(use-package keybindings
  ;;:ensure nil
  ;;:straight nil
  :no-require t
  ;;:straight (:type built-in)
  :after (straight projectile magit lookup helpful consult)
  :config
  (message "test")

  ;; Buffer-specific bindings
  (general-create-definer leader-buffer-def
    :prefix "C-b"
    :states '(normal visual))

  ;; Code specific map
  (general-create-definer leader-code-def
    :prefix "C-c"
    :states '(normal visual))

  ;; General emacs commands such as edit config files
  (general-create-definer leader-emacs-def
    :prefix "C-e"
    :states '(normal visual))

  ;; File-specific bindings
  (general-create-definer leader-file-def
    :prefix "C-f"
    :states '(normal visual))

  ;; Source control commands
  (general-create-definer leader-scm-def
    :prefix "C-g"
    :states '(normal visual))

  ;; Various help and information
  (general-create-definer leader-help-def
    :prefix "C-h"
    :states '(normal visual))

  ;; Major-mode-specific bindings
  (general-create-definer leader-mode-def
    :prefix "C-c"
    :states '(normal visual))

  ;; Project-specific bindings
  (general-create-definer leader-project-def
    :keymaps 'projectile-mode-map
    :prefix "C-p"
    :states '(normal visual))

  ;; Already bound so unbind from various built in packages and evil.el, this
  ;; causes an error during startup where it says that "C-t is not a leader key".
  ;; Unbind all
  (define-key global-map (kbd "C-t") nil)
  (define-key evil-normal-state-map (kbd "C-t") nil)
  (define-key evil-insert-state-map (kbd "C-t") nil)
  (general-create-definer leader-toggle-def
    :prefix "C-t"
    :states '(normal insert visual))

  ;; Window bindings
  (general-create-definer leader-window-def
    :prefix "C-w"
    :states '(normal visual))

  (leader-file-def
   "f" 'find-file)

  (leader-emacs-def
   "c" '(lambda ()
	  (interactive)
	  (find-file (expand-file-name "README.org" user-emacs-directory))))

  (leader-code-def
   "n" 'flycheck-next-error
   "p" 'flycheck-previous-error
   "l" 'flycheck-list-errors)

  (leader-buffer-def
   "d" 'kill-current-buffer)

  (leader-toggle-def
   "l" 'toggle-line-number-mode)

  (leader-mode-def
   :prefix "C-m"
   :keymaps 'smerge-mode-map
   "n" 'smerge-next
   "p" 'smerge-prev
   "d" 'smerge-diff-base
   "u" 'smerge-keep-upper
   "l" 'smerge-keep-lower)


  ;; Projectile
  (leader-project-def
   "a" 'projectile-add-known-project
   "d" 'projectile-remove-known-project
   "p" 'projectile-switch-project
   "f" 'projectile-find-file
   "i" 'projectile-invalidate-cache
   "k" 'projectile-kill-buffer)

  (leader-scm-def
   "s" 'magit-status
   "t" 'magit-todos-list)

  ;; lookup
  (leader-code-def
   "h" #'+lookup/documentation)

  ;; helpful
  (leader-code-def
   "d" #'helpful-at-point)

  (leader-help-def
   "k" #'helpful-key
   "o" #'helpful-symbol
   "v" #'helpful-variable
   "x" #'helpful-command
   "F" #'helpful-function
   "f" #'helpful-callable)


  ;; consult
  (leader-buffer-def
   "b" #'consult-buffer)

  (leader-file-def
   "r" #'consult-recent-file)

  ;; One-off to re-define keys in the stock C-x
  (general-define-key
   :prefix "C-x"
   "b" #'consult-buffer)


  )
