(require 'package)

(setq work-dir "~/.config/custard")
(setq home-dir "~/.emacs.d")
(setq user-emacs-directory
      (if (string-match-p "ard" (system-name))
          work-dir
        home-dir))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)


(setenv "JAVA_HOME"
	(mapconcat 'identity
		   (list (substitute-in-file-name "$HOME")
                         ".sdkman/candidates/java/current")
		   "/"))

(setq-default indent-tabs-mode nil)

(use-package emacs
  :init
  (setq completion-cycle-threshold 2
	auto-revert-verbose nil
	custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
  (load custom-file)
  ;; exports from my ~/.zshenv
  (setq exec-path-from-shell-variables '("ANTHROPIC_API_KEY"
                                         "AWS_ACCESS_KEY"
                                         "AWS_SECRET_ACCESS_KEY"
                                         "CEREBRAS_API_KEY"
                                         "DUCKDB_HOME"
                                         "KAGI_BASE"
                                         "LINEAR_API_KEY"
                                         "NVM_DIR"
                                         "XAI_API_KEY"
                                         "PATH"))
  (exec-path-from-shell-initialize)
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
;; (require 'init-eglot)
(require 'init-elfeed)
(require 'init-eshell)
(require 'init-eww)
(require 'init-flycheck)
(require 'init-gptel)
(require 'init-lisp-stuff)
(require 'init-magit)
(require 'init-nav)
(require 'init-org)
(require 'init-prolog)
(require 'init-transient)
(require 'init-treesit)
(require 'init-vertico)
(require 'init-vundo)
(require 'init-ui)
(require 'init-yas)
