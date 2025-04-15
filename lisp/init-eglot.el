(use-package prettier-js
  :ensure t
  :hook (((js2-mode typescript-mode tsx-ts-mode typescript-ts-mode js2-jsx-mode) . prettier-js-mode)))

(use-package eglot
  :ensure t
  :commands (eglot-managed-p)
  :preface
  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  
  :hook
  (((java-mode
     fsharp-mode
     typescript-ts-mode
     typescript-mode)
    . eglot-ensure)
   ;; ((cider-mode eglot-managed-mode) . eglot-disable-in-cider)
   )
  
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider)))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'init-eglot)
