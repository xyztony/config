(use-package magit
  :ensure t
  :init
  (use-package with-editor :ensure t)
  
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
	magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-magit)
