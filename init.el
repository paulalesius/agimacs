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
  :after evil
  :config

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
    :prefix "C-m"
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
   "l" 'smerge-keep-lower))

(use-package projectile
  :after general
  :config
  (projectile-mode +1)
  (leader-project-def
    "a" 'projectile-add-known-project
    "d" 'projectile-remove-known-project
    "p" 'projectile-switch-project
    "f" 'projectile-find-file
    "i" 'projectile-invalidate-cache
    "k" 'projectile-kill-buffer))

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
  :after evil
  :config
  (leader-scm-def
    "s" 'magit-status
    "t" 'magit-todos-list))

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
  :config
  ;;(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package lookup
  :straight (lookup :type git :host github :repo "aaronjensen/emacs-lookup" :commit "6ffdb61ef7c70077dee45330d4444a0eec559e01")
  :after general
  :config
  (leader-code-def
    "h" #'+lookup/documentation))

(use-package helpful
  :after general
  :config

  (leader-code-def
   "d" #'helpful-at-point)

  (leader-help-def
   "k" #'helpful-key
   "o" #'helpful-symbol
   "v" #'helpful-variable
   "x" #'helpful-command
   "F" #'helpful-function
   "f" #'helpful-callable))

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
  :config

  (leader-buffer-def
   "b" #'consult-buffer)

  (leader-file-def
   "r" #'consult-recent-file)

  ;; One-off to re-define keys in the stock C-x
  (general-define-key
   :prefix "C-x"
   "b" #'consult-buffer))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
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
  :custom
  (flycheck-indication-mode 'right-fringe "Move the indicator to the right fringe to allow git-gutter")
  :after lsp-mode
  ;;:hook (lsp-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
        flycheck-idle-change-delay 0.8)
  :config
  (global-flycheck-mode t))

(use-package rainbow-delimiters)

(use-package format-all
  :straight (:type git :host github :repo "lassik/emacs-format-all-the-code" :commit "22e48b831d64ca1647ae28f9e9485378577ea4f8"))

(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (when (fboundp 'doom-modeline-update-env)
        (doom-modeline-update-env))
      (force-mode-line-update))))

(defun +modeline-clear-env-in-all-windows-h (&rest _)
  "Blank out version strings in all buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq doom-modeline-env--version
              (bound-and-true-p doom-modeline-load-string))))
  (force-mode-line-update t))

(defun setup-python-mode-keybindings()
  (leader-mode-def
    :keymaps 'python-mode-map
    "s" '(:ignore t :which-key "REPL")
    ;; REPL
    "s r" '(python-shell-send-region :which-key "send region")
    "s b" '(python-shell-send-buffer :which-key "send buffer")
    "s f" '(python-shell-send-file :which-key "send file")))

(use-package python
  :mode ("[./]pyproject.toml\\'" . conf-mode)
  :after (general projectile lsp-mode flycheck)
  :hook (python-mode . lsp-deferred)
  :hook (python-mode . #'setup-python-mode-keybindings)
  :custom
  (python-indent-guess-indent-offset-verbose nil "Don't emit warning when indent guessing fails")
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  (add-hook 'python-mode-hook
            (defun +python-use-correct-flycheck-executables-h ()
              "Use the correct Python executables for Flycheck."
              (let ((executable python-shell-interpreter))
        	(save-excursion
        	  (goto-char (point-min))
        	  (save-match-data
        	    (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
        		      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
        	      (setq executable (substring-no-properties (match-string 1))))))
        	;; Try to compile using the appropriate version of Python for
        	;; the file.
        	(setq-local flycheck-python-pycompile-executable executable)
        	;; We might be running inside a virtualenv, in which case the
        	;; modules won't be available. But calling the executables
        	;; directly will work.
        	(setq-local flycheck-python-pylint-executable "pylint")
        	(setq-local flycheck-python-flake8-executable "flake8")))))

(use-package pyvenv
  :after (doom-modeline python)
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-post-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))))

(use-package poetry
  :after (python pyvenv)
  :custom
  (poetry-tracking-strategy 'switch-buffer)
  :init
  (add-hook 'python-mode-hook #'poetry-tracking-mode)
  :config
  (leader-mode-def
    :keymaps 'python-mode-map
    "p" '(:ignore t :which-key "poetry")
    "p p" #'poetry))

(use-package pytest
  :after python
  :config

  (leader-mode-def
   :keymaps 'python-mode-map
   "t" '(:ignore t :which-key "pytest")
   ;; Testing
   "t a" #'pytest-all
   "t m" #'pytest-module
   "t c" #'pytest-one
   "t r" #'pytest-again
   "t d" #'pytest-directory))

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
  (let ((toc-begin-re "# BEGIN_TOC \\([0-9]+\\)")
	(toc-end-re "# END_TOC")
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
					 (mapconcat 'number-to-string current-section-numbers "."))))
		     (anchor (replace-regexp-in-string " " "-" (downcase (replace-regexp-in-string "[^a-zA-Z0-9 -]" "" headline)))))

		(when (<= level max-level)
		  (push (format "- %s [[*%s][%s]]" section-number anchor headline) headlines))))
	    (when toc-end-pos
	      (goto-char toc-begin-pos)
	      (delete-region toc-begin-pos toc-end-pos)
	      (insert "\n" (mapconcat 'identity (nreverse headlines) "\n") "\n")))
	        (message "Warning: No # BEGIN_TOC block found.")))))

(use-package yaml-mode
  :after general
  :mode ("\\.yml\\'" . yaml-mode)
  :mode ("\\.yaml\\'" . yaml-mode)
  :hook (yaml-mode . setup-yaml-mode-keybindings)
  :config
  (defun setup-yaml-mode-keybindings()
    (leader-mode-def
      :keymaps 'yaml-mode-map
      "n" #'newline-and-indent)))

(define-minor-mode agi-mode
  "A minor mode for AGI project."
  :lighter " 🤖"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(use-package yaml
  :straight '(yaml :type git :host github :repo "zkry/yaml.el" :commit "01a12f2345d309fe86770e0a61a7f26f47a2cd0a"))

(defun parse-agi-yaml ()
  "Parse the agi.yaml file in the project root and return the parsed content."
  (let* ((project-root (projectile-project-root))
         (agi-yaml-path (concat project-root "agi.yaml")))
    (when (and project-root (file-exists-p agi-yaml-path))
      (with-temp-buffer
        (insert-file-contents agi-yaml-path)
        (yaml-parse-string (buffer-string)
           		   :object-type 'alist
           		   :sequence-type 'list)))))

(defun agi-command-func-generator (executable args)
  "Return a function that when called, will execute the specified EXECUTABLE with the specified ARGS."
  (lambda ()
    (interactive)
    (apply 'call-process executable nil 0 nil args)))

(defun setup-agi-commands ()
  "Setup AGI commands from agi.yaml."
  (let ((commands-alist (cdr (assoc 'commands (parse-agi-yaml))))
        (counter 1))
    (dolist (command commands-alist)
      (when (<= counter 9)
        (let ((name (cdr (assoc 'name command)))
              (executable (cdr (assoc 'executable command)))
              (arguments (cdr (assoc 'arguments command))))
          (let ((func (agi-command-func-generator executable arguments)))
            (fset (intern (concat "agi-command-" name)) func)
            (leader-mode-def
              :prefix "C-m"
              :keymaps 'agi-mode-map
              (format "a %d" counter) (intern (concat "agi-command-" name)))
            (message "Setting up command: %s" name))
          (setq counter (1+ counter)))))))

(defun agi-project-p ()
  "Return non-nil if the current buffer is in an AGI project."
  (and (projectile-project-p)
       (locate-dominating-file (projectile-project-root) "agi.yaml")))

(defun agi-mode-maybe-activate ()
  "Activate `agi-mode` if the current buffer is in an AGI project."
  (when (agi-project-p)
    (agi-mode 1)
    (setup-agi-commands)))

(add-hook 'find-file-hook 'agi-mode-maybe-activate)
