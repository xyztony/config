(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-clojure-custom-server-command '("/opt/homebrew/bin/clojure-lsp")
	lsp-eslint-package-manager "pnpm"
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-icons-enable nil
	lsp-keymap-prefix "C-.")
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . lsp-deferred)
	 (clojure-mode . lsp)
	 (clojurec-mode . lsp)
	 (clojurescript-mode . lsp)
         (java-mode . lsp)
	 ((tsx-ts-mode typescript-ts-mode js-ts-mode) . lsp-deferred))
  :commands (lsp lsp-deferred)
  :bind
  (:map lsp-mode-map
	("s-l f" . lsp-find-definition)
	("s-l d" . lsp-find-references)
        ("C-t C-t" . lsp-treemacs-symbols)))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-java
  :ensure t)

(provide 'init-lsp)
