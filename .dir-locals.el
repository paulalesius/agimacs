((org-mode . ((eval . (progn
    			(defvar-local my-readme-onsave-hook-guard nil)
    			(defun my-readme-onsave-hook-payload ()
    			  (org-babel-tangle)
    			  (insert-org-mode-toc))
                          (defun my-readme-onsave-hook ()
                            "Org tangle triggers onsave again, causing an infinite loop. Place a buffer-local
                             guard to prevent recursion."
    			  (unless my-readme-onsave-hook-guard
    			    (setq my-readme-onsave-hook-guard t)
    			    (my-readme-onsave-hook-payload)
    			    (setq my-readme-onsave-hook-guard nil)))
                          (add-hook 'before-save-hook 'my-readme-onsave-hook nil t))))))
