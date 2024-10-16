(use-package treesit
  :mode (("\\.clj\\'" . clojure-ts-mode)
	 ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode))
  :hook ((clojure-ts-mode . cider-mode)
	 (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp))))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
	       (css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
	       (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (mapping
           '((python-mode . python-ts-mode)
	     (clojure-mode . clojure-ts-mode)
	     (clojure-script-mode . clojure-ts-mode)
             (css-mode . css-ts-mode)
	     ;; (emacs-lisp-mode . elisp-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package treesit-fold
  :load-path "treesit-fold"
  :config
  (global-treesit-fold-indicators-mode 1))

(use-package treesit-fold-indicators
  :load-path "treesit-fold")

(require 'treesit)

(defun my/goto-top-object ()
  "Move point to the beginning of the topmost object."
  (interactive)
  (let* ((p (point))
	 (cursor (tsc-make-cursor (treesit-node-at p (treesit-language-at p))))
	 (node (tsc-current-node cursor)))
    (while (or
	    (equal (tsc-node-type (tsc-get-parent node)) 'object)
	    (equal (tsc-node-type (tsc-get-parent node)) 'pair))
      (message "Current node: %s" (tsc-node-text node))
      (setq node (tsc-get-parent node)))
    (goto-char (tsc-node-start-position node))))




(provide 'init-treesit)
