(require 'clj-refactor)

(use-package cider
  :ensure t
  :init
  :config
  (cider-enable-flex-completion)
  (setq cider-show-error-buffer 'only-in-repl
	cider-clojure-compilation-error-phases nil
	cider-repl-display-help-banner nil
	cider-stacktrace-fill-column 80
	cider-use-overlays t)
  :bind
  (:map cider-mode-map
	("C-x C-e" . cider-eval-dwim)
	("C-x C-r" . cider-pprint-eval-last-sexp-to-repl)
	("C-c C-a" . cider-inspect-last-result)
	("C-c f d" . cider-find-dwim)
	("C-c f n" . cider-find-ns)
	("C-c f k" . cider-find-key))
  (:map cider-repl-mode-map
	("C-c M-l" . cider-repl-clear-buffer)))


(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clojure-mode
  :custom
  (setq clojure-indent-style 'always-indent
      clojure-indent-keyword-style 'always-indent
      clojure-enable-indent-specs nil)
  :hook
  ((clojure-mode . my-clojure-mode-hook)))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

(provide 'init-clojure)
