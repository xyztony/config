(use-package magit
  :ensure t
  :init
  (use-package with-editor :ensure t)
  
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
	magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

(provide 'init-magit)
