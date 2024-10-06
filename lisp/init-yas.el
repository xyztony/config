(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config
  (add-to-list 'load-path "~/.emacs.d/snippets"))

(provide 'init-yas)
