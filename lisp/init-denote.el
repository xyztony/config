(use-package denote
  :ensure t
  
  :config
  (setq denote-templates `(("thoughts" .
			    ,(concat "* Some thoughts on ..."
				     "\n\n"
				     "- ...")))))

(defmacro projectile-with-denote-dir (&rest body)
  "Execute body with projectile's default directory set to `denote-directory'."
  `(let ((projectile-require-project-root nil))
     (projectile-with-default-dir denote-directory
       ,@body)))

(defun search-denote-directory (regexp)
  "Search for REGEXP in the denote directory using projectile."
  (interactive "sSearch notes for (regexp):")
  (projectile-with-denote-dir
   (let ((results-buffer (projectile-ripgrep regexp)))
     (when results-buffer
       (switch-to-buffer results-buffer)))))

(defun open-denote-directory-in-projectile-dired ()
  "Open denote directory using projectile-dired, showing all org-files"
  (interactive)
  (projectile-with-denote-dir
   (let ((dired-buffer (projectile-dired)))
     (with-current-buffer dired-buffer
       (revert-buffer)
       (dired-mark-files-regexp "\\.org$")
       (dired-toggle-marks)
       (dired-mark-files-regexp "^#.*#$")
       (dired-mark-files-regexp "^\\.#")
       (dired-do-kill-lines))
     (switch-to-buffer dired-buffer))))

(provide 'init-denote)
