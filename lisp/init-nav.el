(use-package avy
  :bind
  (:map global-map
        ("C-c j c" . avy-goto-char)
        ("C-c j l" . avy-goto-line)
        ("C-c j m l" . avy-move-line)
        ("C-c j m r" . avy-move-region)))

(use-package dired
  :after dired
  :bind
  (:map dired-mode-map
	("C-c C-n" . dired-create-empty-file)
        ("C-c C-a" . gptel-add)))

(use-package wgrep
  :ensure t)

(provide 'init-nav)
