(require 'init-utils)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-count 10
	vertico-resize t
	vertico-cycle nil))

(use-package vertico-flat
  :after vertico
  :ensure nil
  :init
  (vertico-flat-mode)
  :bind
  (:map vertico-flat-map
	("TAB" . vertico-next)
	([tab] . vertico-next)
	([backtab] . vertico-previous)
	("S-TAB" . vertico-previous)))

(use-package vertico-prescient
  :after vertico
  :init
  (vertico-prescient-mode))

(defgroup affe-extensions nil
  "Customizations for extending affe."
  :group 'affe)

(defcustom affe-grep-ignore-globs
  '(;;
    "site-lisp/"
    "**/data"
    "**/dist"
    "**/node_modules"
    "**/package-lock.json"
    ".clj-kondo/"
    "elpa/"
    "**/*.transit.json"
    "History/**"
    "**/#*#.*"
    "**/.aider*"
    "**/bikeshed*"
    "gptel/"
    "treesit-fold/")
  "Glob patterns to ignore!"
  :type '(repeat string)
  :group 'affe-extensions)

(use-package affe
  :vc (:url "https://github.com/minad/affe" :rev :newest)
  :config
  (consult-customize affe-grep :preview-key "M-.")
  (setq affe-grep-command
        (concat "rg "
                (string-join
                 (mapcan (lambda (glob) (list "-g" (concat "!" glob)))
                         affe-grep-ignore-globs) " ")
                " --null --no-heading --line-number -v ^$"))
  (advice-add 'affe-grep :around #'ant/with-vertico-mode)
  (advice-add 'affe-find :around #'ant/with-vertico-mode))

(advice-add 'consult-ripgrep :around #'ant/with-vertico-mode)

(provide 'init-vertico)
