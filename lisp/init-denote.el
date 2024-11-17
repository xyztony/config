(use-package denote
  :ensure t
  
  :config
  (setq denote-templates `(("thoughts" .
			    ,(concat "* Some thoughts on ..."
				     "\n\n"
				     "- ...")))))

(defmacro with-denote-dir (&rest body)
  "Execute BODY with the current project directory set to `denote-directory', then reset it."
  `(prog1
       (let ((project-current-directory-override denote-directory))
         ,@body)
     (setq project-current-directory-override nil)))

(defun search-denote-directory (regexp)
  "Search for REGEXP in the denote directory."
  (interactive "sSearch notes for (regexp): ")
  (with-denote-dir
   (project-find-regexp regexp)))

(defun open-denote-directory-in-dired ()
  "Open denote directory using projectile-dired, showing all org-files"
  (interactive)
  (with-denote-dir
   (dired project-current-directory-override)
   (dired-mark-files-regexp "\\.org$")
   (dired-toggle-marks)
   (dired-mark-files-regexp "^#.*#$")
   (dired-mark-files-regexp "^\\.#")
   (dired-do-kill-lines)))

(provide 'init-denote)
