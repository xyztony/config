(use-package clj-refactor
  :ensure t
  :vc (:url "https://github.com/clojure-emacs/clj-refactor.el"
            :rev :newest)
  :hook (clojure-mode . clj-refactor-mode))

(use-package cider
  :ensure t
  :config
  (cider-enable-flex-completion)
  (setq cider-clojure-compilation-error-phases nil
	cider-repl-display-help-banner nil
	cider-stacktrace-fill-column 80
        cider-save-file-on-load t
        cider-use-overlays t
        cider-show-error-buffer nil
        cider-auto-jump-to-error nil)
  :bind
  (:map cider-mode-map
	("C-x C-e" . cider-eval-dwim)
	("C-x C-r" . cider-pprint-eval-last-sexp-to-repl)
        ("C-x C-p" . cider-eval-print-last-sexp)
        ("C-c C-a" . cider-inspect-last-result)
	("C-c f d" . cider-find-dwim)
	("C-c f n" . cider-find-ns)
	("C-c f k" . cider-find-key)
        ("C-c M-b" . cider-format-buffer)
        ("C-c M-f" . cider-format-defun))
  (:map cider-repl-mode-map
	("C-c M-l" . cider-repl-clear-buffer)
        ("C-c M-l" . cider-repl-clear-buffer)
	("C-c C-a" . cider-inspect-last-result)
        ("C-c X" . cider-selector)
        ("M-RET" . clerk-show)))

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :custom
  (setq clojure-indent-style 'always-indent
        clojure-indent-keyword-style 'always-indent
        clojure-enable-indent-specs nil)
  :config
  (require 'flycheck-clj-kondo)
  :hook
  (clojure-mode . my-clojure-mode-hook)
  (buffer-save-hook . cider-format-buffer)
  :bind
  (:map clojure-mode-map
        ("C-c C-r n" . clojure-cycle-not)
        ("C-c C-r w" . clojure-cycle-when)
        ("C-c C-r i" . clojure-cycle-if)
        ("C-c C-r ;" . clojure-toggle-ignore-surrounding-form)))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(provide 'init-clojure)
