(use-package dired
  :after dired
  :bind
  (:map dired-mode-map
	("C-c C-n" . dired-create-empty-file)))



(use-package projectile
  :init (projectile-mode +1)
  :config
  (setq dired-hide-details-hide-information-lines nil)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

(use-package wgrep
  :ensure t)

(provide 'init-nav)
