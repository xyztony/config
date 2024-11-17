(use-package dired
  :after dired
  :bind
  (:map dired-mode-map
	("C-c C-n" . dired-create-empty-file)
        ("C-c C-a" . gptel-add)))

(use-package wgrep
  :ensure t)

(provide 'init-nav)
