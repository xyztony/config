(add-hook 'after-init-hook #'dired-jump)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default initial-buffer-choice "~/Developer")
(setq-default inhibit-startup-screen nil)
(setq-default initial-scratch-message nil)
(setq-default frame-resize-pixelwise t)
(setq-default startup-screen-inhibit-startup-screen 't)
(setq-default visible-bell nil)
(setq-default ring-bell-function 'ignore)
(setq-default display-line-numbers 'relative)
(setq-default highlight-nonselected-windows 't)

(setenv "JAVA_HOME"
	(mapconcat 'identity (list (substitute-in-file-name "$HOME") ".sdkman/candidates/java/current") "/"))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(let ((uninstalled-pkgs (seq-filter
			 (lambda (x)
			   (not (package-installed-p x)))
			 '(cider
			   clojure-mode
			   corfu
			   corfu-prescient
			   edit-indirect
			   exec-path-from-shell
			   lsp-mode
			   magit
			   marginalia
			   org-roam
			   prescient
			   projectile
			   projectile-ripgrep
			   puni
			   vertico
			   vertico-prescient
			   vterm
			   ))))
  (when (and (not (equal uninstalled-pkgs '()))
	     (y-or-n-p (format "Install the following packages: %s?" uninstalled-pkgs)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-pkgs)))


(global-set-key (kbd "C-c C-l") 'load-file)

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

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  :hook
  (cider-repl-mode . corfu-mode)
  (cider-mode . corfu-mode))

(use-package corfu-prescient
  :after corfu
  :init
  (corfu-prescient-mode))

(use-package dired
  :after dired
  :bind
  (:map dired-mode-map
	("C-c C-n" . dired-create-empty-file)))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3
	tab-always-indent 'complete))

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-clojure-custom-server-command '("/opt/homebrew/bin/clojure-lsp"))
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  
  :hook ((clojure-mode . lsp-deferred)
	 (clojurec-mode . lsp-deferred)
	 (clojurescript-mode . lsp-deferred))
  :commands (lsp lsp-deferred))


(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-n" . marginalia-cycle))

  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)

  :init
  (marginalia-mode))

(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "Agave Nerd Font Mono" :height 200)
(set-face-attribute 'fixed-pitch nil :font "Agave Nerd Font Mono" :height 200)

(require 'org-faces)

(use-package org-roam
  :ensure t
  :init
  (org-roam-db-autosync-mode))

(dolist (face '(window-divider
		window-divider-first-pixel
		window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
  ;; Edit settings
  org-auto-align-tags nil
  org-tags-column 0
  org-catch-invisible-edits 'show-and-error
  org-special-ctrl-a/e t
  org-insert-heading-respect-content t
  org-use-speed-commands t

  ;; Org styling, hide markup etc.
  org-hide-emphasis-markers t
  org-pretty-entities t
  org-ellipsis "…"

  ;; LSP
  gc-cons-threshold (* 100 1024 1024)
  read-process-output-max (* 1024 1024)
  company-minimum-prefix-length 1
  lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
  lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
  )

;; Resize Org headings
(dolist (face '((org-level-1 . 1.4)
		(org-level-2 . 1.2)
		(org-level-3 . 1.1)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Victor Mono" :weight 'medium :height (cdr face)))

(use-package prescient
  :config
  (setq prescient-filter-method '(regexp fuzzy prefix)))

(use-package projectile
  :init (projectile-mode +1)
  :config
  (setq dired-hide-details-hide-information-lines nil)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

(use-package puni
  :defer t
  :init (puni-global-mode)
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  (term-mode-hook . puni-disable-puni-mode)
  :bind
  (:map puni-mode-map
	("C-M-s" . puni-mark-sexp-at-point)
	("C-M-a" . puni-mark-sexp-around-point)
	("C-M-d" . puni-mark-list-around-point)

	("C-M-h" . puni-slurp-backward)
	("C-M-l" . puni-slurp-forward)

	("C-M-b" . puni-barf-backward)
	("C-M-n" . puni-barf-forward)

	("C-M-p j" . puni-split)
	("C-M-p s" . puni-beginning-of-sexp)
	("C-M-p e" . puni-end-of-sexp)
	("C-M-p c" . puni-wrap-curly)
	("C-M-p r" . puni-wrap-round)
	("C-M-p x" . puni-force-delete)))

(use-package tab-bar
  :config
  (setq tab-bar-new-tab-choice 'directory)
  (keymap-set tab-prefix-map "e" '(lambda () (interactive) (find-file-other-tab (expand-file-name "init.el" user-emacs-directory))))
  (keymap-set tab-prefix-map "w" '(lambda () (interactive) (dired-other-tab "~/documents/nurts/writings")))
  (keymap-set tab-prefix-map "q" '(lambda () (interactive) (dired-other-tab "~/repos"))))

(use-package transient
  :ensure t
  :config
  (setq transient-show-popup 0.15)
  (setq transient-detect-key-conflicts t)
  (setq transient-default-level 5)
  (transient-bind-q-to-quit)
  
  ;; Magit transient
  (transient-define-prefix transient-magit-menu ()
    "Magit menu"
    [["Status"
      ("s" "Status" magit-status)
      ("g" "Magit" magit)]])

  ;; Search and Replace transient
  (transient-define-prefix transient-search-replace-menu ()
    "Search and Replace menu"
    [["Search"
      ("s" "Search forward" isearch-forward)
      ("r" "Search backward" isearch-backward)
      ("o" "Occur" occur)]
     ["Replace"
      ("%" "Query replace" query-replace)
      ("M-%" "Query replace regexp" query-replace-regexp)]
     ["Edit"
      ("p" "Kill paragraph" kill-paragraph)]
     ["Grep"
      ("g" "Grep" grep)
      ("G" "Recursive grep" rgrep)]])

  (transient-define-prefix transient-window-management ()
    "Window management menu"
    [["Navigation"
      ("n" "Next window" other-window)
      ("p" "Previous window" (lambda () (interactive) (other-window -1)))]
     ["Split"
      ("-" "Split below" split-window-below)
      ("/" "Split right" split-window-right)]
     ["Delete"
      ("q" "Delete window" delete-window)
      ("d" "Delete other windows" delete-other-windows)]
     ["Resize"
      ("B" "Shrink horizontally" shrink-window-horizontally)
      ("F" "Enlarge horizontally" enlarge-window-horizontally)
      ("N" "Shrink vertically" shrink-window)
      ("P" "Enlarge vertically" enlarge-window)]
     ["Balance"
      ("=" "Balance windows" balance-windows)]])

  (transient-define-prefix transient-org-roam-menu ()
    "Org roam menu"
    [["Files"
      ("f" "Find file" org-roam-node-find)
      ("c" "Capture node" org-roam-capture)
      ("tt" "Capture today" org-roam-dailies-capture-today)
      ("tm" "Capture tomorrow" org-roam-dailies-capture-tomorrow)]])


  :bind
  ("C-c w" . transient-window-management)
  ("C-c s" . transient-search-replace-menu)
  ("C-c o" . transient-org-roam-menu)
  ("C-c g" . transient-magit-menu))

(use-package vertico
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" default))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(vterm marginalia meow projectile-ripgrep edit-indirect projectile flutter dart-mode exec-path-from-shell markdown-mode evil-org prescient magit vertico-prescient kaolin-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'magit-diff-edit-hunk-commit 'disabled nil)
