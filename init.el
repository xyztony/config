;;; init --- ...
;;; Commentary:
;;; setup path, directory, etc
(require 'package)

(setq enable-local-eval nil)
(setq ns-use-native-fullscreen nil)

(defconst work-dir "~/.config/custard")
(defconst home-dir "~/.emacs.d")
(defun work-machine-p ()
  (or (string-match-p "ard" (system-name))
      (string-match-p "Mac.home" (system-name))))

(setq user-emacs-directory
      (if (work-machine-p)
          work-dir
        home-dir))

(defun add-to-load-path (dir)
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(add-to-load-path "lisp")
(add-to-load-path "gptel")
(package-install-file (expand-file-name "gptel" user-emacs-directory))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(setenv "JAVA_HOME"
	(mapconcat 'identity
		   (list (substitute-in-file-name "$HOME")
                         ".sdkman/candidates/java/current")
		   "/"))

(setq-default indent-tabs-mode nil)
(setq-default ns-use-native-fullscreen nil)

(use-package emacs
  :custom
  (mac-option-key-is-meta t)
  (mac-option-modifier 'meta)
  (make-backup-files nil)
  (create-lockfiles nil)
  (completion-cycle-threshold 2)
  (auto-revert-verbose nil)
  (custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
  (exec-path-from-shell-variables '("ANTHROPIC_API_KEY"
                                    "AWS_ACCESS_KEY"
                                    "AWS_SECRET_ACCESS_KEY"
                                    "CEREBRAS_API_KEY"
                                    "DUCKDB_HOME"
                                    "GEMINI_API_KEY"
                                    "KAGI_BASE"
                                    "GEMINI_API_KEY"
                                    "LINEAR_API_KEY"
                                    "NVM_DIR"
                                    "OPENROUTER_API_KEY"
                                    "XAI_API_KEY"
                                    "PATH"))
  (global-auto-revert-mode 1)
  :init
  (when (not (work-machine-p))
    (load custom-file))
  (exec-path-from-shell-initialize)
  (unbind-key "s-p")
  (unbind-key "M-j")
  :bind
  (("<C-return>" . newline-and-indent)
   ("C-x C-r" . query-replace)
   ("C-c C-l" . load-file)
   ("C-j" . join-line)
   ("M-h" . mark-paragraph)
   ("M-s M-s" . ispell-word)
   ("C-c C-r" . consult-ripgrep)
   ("C-a" . ant/beginning-of-line)
   ("C-c C-/" . vundo)
   ("s-b" . switch-to-buffer)
   ("s-p" . project-find-file)
   ("s-f" . affe-find)
   ("s-g" . affe-grep)))

(require 'init-clojure)
(require 'init-corfu)
(require 'init-denote)
(when (not (work-machine-p))
  (require 'init-elfeed))
(require 'init-eglot)
(require 'init-envrc)
(require 'init-eshell)
(require 'init-eww)
(require 'init-flycheck)
(require 'init-gptel)
(require 'init-julia)
(require 'init-lisp-stuff)
;; (require 'init-lsp)
(require 'init-magit)
(require 'init-nav)
(require 'init-org)
(require 'init-prolog)
(require 'init-rotate)
(require 'init-transient)
(require 'init-treesit)
(require 'init-ui)
(require 'init-vertico)
(require 'init-vterm)
(require 'init-vundo)
(require 'init-weblorg)
(require 'init-webstuff)
