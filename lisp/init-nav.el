(use-package dired
  :after dired
  :bind
  (:map dired-mode-map
	("C-c C-n" . dired-create-empty-file)))

(use-package wgrep
  :ensure t)

(provide 'init-nav)
