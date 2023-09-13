(when (string-equal (getenv "EMACS_PROFILE") "true")
  (setq garbage-collection-messages t)
  (profiler-start 'cpu+mem))

(setq package-enable-at-startup nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

(setq old-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold old-gc-cons-threshold)))
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Ready for business. Startup in %s with %d gcs."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string-equal (getenv "EMACS_PROFILE") "true")
              (profiler-stop)
              (let ((profiler-report-file (expand-file-name "profiler-report.txt" user-emacs-directory)))
                (with-current-buffer (profiler-report-cpu)
                  (write-file profiler-report-file))
                (message "Profiler report saved to %s" profiler-report-file)))))

(setq straight-use-package-by-default t
      straight-process-buffer " *straight-process*")

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
