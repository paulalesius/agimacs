(setq package-enable-at-startup nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

(setq old-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold old-gc-cons-threshold)))
;; Startup hook from https://config.daviwil.com/emacs
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Ready for business. Startup in %s with %d gcs."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(font-lock-add-keywords 'emacs-lisp-mode
                      '(("(\\(straight-use-package\\)\\_>"
                         (1 'font-lock-keyword-face))))

(straight-use-package 'use-package)
