(use-package cider
  :ensure t
  :init
  :config
  (cider-enable-flex-completion)
  :custom
  (setq cider-show-error-buffer 'only-in-repl
	cider-clojure-compilation-error-phases nil
	cider-repl-display-help-banner nil
	cider-stacktrace-fill-column 80)
  :bind
  (:map cider-mode-map
	("C-x C-e" . cider-eval-dwim)
	("C-x C-r" . cider-pprint-eval-last-sexp-to-repl)
	("C-c M-l" . cider-repl-clear-buffer)
	("C-c C-a" . cider-inspect-last-result)))

(use-package clojure-mode
  :custom
  (setq clojure-indent-style 'always-indent
      clojure-indent-keyword-style 'always-indent
      clojure-enable-indent-specs nil))

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
