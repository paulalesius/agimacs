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
  :after (evil magit)
  :custom
  (evil-collection-setup-minibuffer t)
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

    ;; buffer
    "bd" 'kill-current-buffer))

(use-package projectile
  :after general
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
  :after evil
  :config
  (my-leader-def
    "g" '(:ignore t :which-key "magit")  
    "g g" 'magit-status))

(use-package magit-todos
  :after magit
  :custom
  (magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?" "Allow TODO without colons TODO:"))

(use-package dashboard
  ;;:init
  ;;(setq dashboard-startup-banner '((expand-file-name "1.txt" user-emacs-directory)))
  ;;(setq dashboard-startup-banner '("/home/noname/.emacs.custom/1.txt" . ""))
  :config
  ;; Set initial buffer when creating new frames.
  ;; Note: Disabled, creates dashboard buffer when using emacsclient
  ;;(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package helpful
  :after general
  :config
  ;; Declare
  (my-leader-def
    "h" '(:ignore t :which-key "helpful")
    "h k" '(helpful-key :which-key "describe key")
    "h m" '(describe-mode :which-key "describe mode")
    "h v" '(helpful-variable :which-key "describe variable")
    ;; describe-function includes both macros and functions, so
    ;; describe callable is a replacement that includes both
    ;; helpful-callable and helpful-macro
    "h f" '(helpful-callable :which-key "describe callable")
    "h x" '(helpful-command :which-key "describe command"))
  (general-define-key
   :prefix "C-c"
   "C-d" #'helpful-at-point)
  (general-define-key
   :prefix "C-h"
   "F" #'helpful-function))

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
  ;; Re-define keys
  (general-define-key
   :prefix "C-x"
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
  ;; I dont' know what code actions are
  ;;(lsp-ui-sideline-show-code-actions t)
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

(use-package rainbow-delimiters)

(straight-use-package '(format-all :type git :host github :repo "lassik/emacs-format-all-the-code"))

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

(use-package pyvenv
  :after (modeline python)
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-post-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))))

(use-package poetry
  :after python
  :custom
  (poetry-tracking-strategy 'switch-buffer)
  :init
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package pytest
  :after python
  :config
  (my-leader-def
    :keymaps 'python-mode-map
    "m t" '(:ignore t :which-key "pytest")
    ;; Testing
    "m t a" #'pytest-all
    "m t m" #'pytest-module
    "m t c" #'pytest-one
    "m t r" #'pytest-again
    "m t d" #'pytest-directory
    ))

(use-package rustic
  :after (flycheck org lsp-mode rainbow-delimiters)
  :mode ("\\.rs$" . rustic-mode)
  :mode ("^Cargo\\.toml$" . rustic-mode)
  :preface
  (setq rustic-lsp-client nil)
  (with-eval-after-load 'rustic-lsp-client
    (remove-hook 'rustic-mode-hook 'rustic-setup-lsp))
  (with-eval-after-load 'rustic-flycheck
    (remove-hook 'rustic-mode-hook #'flycheck-mode)
    (remove-hook 'rustic-mode-hook #'flycheck-mode-off)
    (remove-hook 'flycheck-mode-hook #'rustic-flycheck-setup))
  (add-hook 'rustic-mode-hook #'rainbow-delimiters-mode)
  (setq rustic-indent-method-chain t)
  (setq rust-prettify-symbols-alist nil)
  (setq rustic-babel-format-src-block nil
	rustic-format-trigger nil)
  (setq rustic-lsp-client 'lsp-mode)
  (add-hook 'rustic-mode-local-vars-hook #'rustic-setup-lsp 'append))

(use-package org
  :custom
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-enforce-todo-dependencies t)
  ;; Defaults to showeverything, but that doesn't respect `org-hide-block-startup'
  ;; (#+startup: hideblocks)`, archive trees, hidden drawers, or VISIBILITY properties. nil
  ;; is equivalent, but respects these settings.
  (org-startup-folded nil))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :hook (doom-docs-org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun insert-org-mode-toc ()
    (interactive)
    (let ((toc-begin-re "#\\+BEGIN_TOC headlines \\([0-9]+\\)")
  	(toc-end-re "#\\+END_TOC")
  	(headlines '())
  	(current-section-numbers ()))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward toc-begin-re nil t)
  	  (let ((max-level (string-to-number (match-string 1)))
  		(toc-begin-pos (match-end 0))
  		(toc-end-pos (if (re-search-forward toc-end-re nil t)
  				 (match-beginning 0)
  			       nil)))
  	    (goto-char (point-min))
  	    (while (re-search-forward "^\\(*+\\) \\(.*\\)" nil t)
  	      (let* ((level (length (match-string 1)))
  		     (headline (match-string 2))
  		     (section-number (if (> level (length current-section-numbers))
  					 (progn
  					   (setq current-section-numbers (append current-section-numbers (list 1)))
  					   (mapconcat 'number-to-string current-section-numbers "."))
  				       (progn
  					 (setcar (nthcdr (- level 1) current-section-numbers)
  						 (+ 1 (nth (- level 1) current-section-numbers)))
  					 (setq current-section-numbers (cl-subseq current-section-numbers 0 level))
  					 (mapconcat 'number-to-string current-section-numbers ".")))))
  		(when (<= level max-level)
  		  (push (format "- %s [[*%s][%s]]" section-number headline headline) headlines))))
  	    (when toc-end-pos
  	      (goto-char toc-begin-pos)
  	      (delete-region toc-begin-pos toc-end-pos)
  	      (insert "\n" (mapconcat 'identity (nreverse headlines) "\n") "\n")))
  	(message "Warning: No #+BEGIN: toc block found."))))
)
