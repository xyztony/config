(require 'package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)


(setenv "JAVA_HOME"
	  (mapconcat 'identity
		     (list (substitute-in-file-name "$HOME")
			   ".sdkman/candidates/java/current")
		     "/"))

(package-initialize)
(let ((uninstalled-pkgs (seq-filter
			 (lambda (x)
			   (not (package-installed-p x)))
			 '(cider
			   clojure-mode
			   clojure-ts-mode
			   corfu
			   corfu-prescient
			   dap-mode
			   denote
			   docker
			   edit-indirect
			   exec-path-from-shell
			   f
			   lsp-java
			   lsp-mode
			   magit
			   prescient
			   projectile
			   projectile-ripgrep
			   puni
			   symbol-overlay
			   vertico
			   vertico-prescient
			   vundo
			   vterm
			   wgrep
			   yasnippet
			   ))))
  (when (and (not (equal uninstalled-pkgs '()))
	     (y-or-n-p (format "Install the following packages: %s?" uninstalled-pkgs)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-pkgs)))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3
	tab-always-indent 'complete
	auto-revert-verbose nil
	custom-file "~/.emacs.d/emacs-custom.el")
  (load custom-file)
  
  :config
  (global-auto-revert-mode 1)
  
  
  :bind
  (("<C-return>" . newline-and-indent)
   ("C-x C-r" . query-replace)
   ("C-c C-l" . load-file)
   ("M-h" . mark-paragraph)
   ("C-c C-/" . vundo)))

(require 'init-clojure)
(require 'init-corfu)
(require 'init-denote)
(require 'init-docker)
(require 'init-lisp-stuff)
(require 'init-lsp)
(require 'init-nav)
(require 'init-symbol)
(require 'init-tabs)
(require 'init-transient)
(require 'init-treesit)
(require 'init-vertico)
(require 'init-vundo)
(require 'init-ui)
(require 'init-yas)
