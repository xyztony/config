(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config
  (add-to-list 'load-path (expand-file-name "snippets" user-emacs-directory)))

(provide 'init-yas)
