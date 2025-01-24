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
         ("\\.pl\\'" . prolog-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode))
  :hook ((clojure-ts-mode . '(cider-mode clojure-mode))
	 (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp))))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
               (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
               (c . ("https://github.com/tree-sitter/tree-sitter-c"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src"))
               (prolog . ("https://github.com/foxyseta/tree-sitter-prolog"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (mapping
           '((clojure-mode . clojure-ts-mode)
	     (clojure-script-mode . clojure-ts-mode)
             (css-mode . css-ts-mode)
	     (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (prolog-mode . prolog-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package treesit-fold
  :load-path "treesit-fold")


(use-package treesit-fold-indicators
   :load-path "treesit-fold")

(provide 'init-treesit)
