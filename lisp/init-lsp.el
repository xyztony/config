(use-package lsp-mode
  :defer t
  :custom
  (lsp-clojure-custom-server-command '("/opt/homebrew/bin/clojure-lsp"))
  (lsp-lens-debounce-interval 0.1)
  (lsp-idle-delay 1.0)
  (lsp-log-io nil)
  (lsp-eslint-package-manager "pnpm")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-keymap-prefix "C-.")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-completion-provider :none)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  :config
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :hook (;;(lsp-mode . lsp-diagnostics-mode)
         ;;(lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . lsp-deferred)
	 (clojure-mode . lsp)
	 (clojurec-mode . lsp)
	 (clojurescript-mode . lsp)
         ((tsx-ts-mode typescript-ts-mode js-ts-mode) . lsp-deferred))
  :commands (lsp lsp-deferred lsp-find-definition lsp-find-references)
  :bind
  (:map lsp-mode-map
	("s-l f" . lsp-find-definition)
	("s-l d" . lsp-find-references)))

;; (use-package lsp-completion
;;   :no-require)

(use-package lsp-java
  :defer t)

(provide 'init-lsp)
