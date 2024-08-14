(add-hook 'after-init-hook #'dired-jump)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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
			   prescient
			   projectile
			   projectile-ripgrep
			   puni
			   vertico
			   vertico-prescient
			   ))))
  (when (and (not (equal uninstalled-pkgs '()))
	     (y-or-n-p (format "Install the following packages: %s?" uninstalled-pkgs)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-pkgs)))

;; Add all my custom packages &/or submodules to 'load-path
(when-let* ((my-dir (expand-file-name "tony" user-emacs-directory))
	    (dir-exists (file-directory-p my-dir))
	    (default-directory my-dir))
  (progn
    (prin1 (format "Dir: %s, exists: %s\n" default-directory dir-exists))
    (normal-top-level-add-subdirs-to-load-path)))

(use-package cider
  :ensure t
  :init
  :config
  (cider-enable-flex-completion)
  :custom
  (setq cider-show-error-buffer 'only-in-repl
	cider-clojure-compilation-error-phases nil
	cider-stacktrace-fill-column 80))

(use-package clojure-mode
  :custom
  (setq clojure-indent-style 'always-indent
      clojure-indent-keyword-style 'always-indent
      clojure-enable-indent-specs nil)
  :hook
  (clojure-mode . 'lsp)
  (clojurec-mode . 'lsp)
  (clojurescript-mode . 'lsp))

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

;; (require 'evil)
;; (evil-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; (global-set-key (kbd "C-c o a") #'org-agenda)

;; (use-package evil-org
;;   :after org
;;   :hook (org-mode . evil-org-mode)
;;   :config
;;   (require 'evil-org-agenda)
;;   (keymap-set org-mode-map "C-c a i" 'org-insert-item)
;;   (keymap-set org-mode-map "C-c a h" 'org-insert-heading)
;;   (keymap-set org-mode-map "C-c e h" 'org-edit-headline)
;;   (keymap-set org-mode-map "C-c l s" 'org-store-link)
;;   (keymap-set org-mode-map "C-c l i" 'org-insert-link)
;;   (setq org-agenda-inhibit-startup t
;; 	org-agenda-use-tag-inheritance nil
;; 	org-agenda-dim-blocked-tasks nil
;; 	org-agenda-files '("~/documents/agenda")))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3
	tab-always-indent 'complete))

(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "Victor Mono" :weight 'light :height 140)
(set-face-attribute 'fixed-pitch nil :font "Victor Mono" :weight 'light :height 140)

(require 'org-faces)

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
	("C-M-a" . puni-mark-sexp-around-point)
	("C-M-h" . puni-slurp-backward)
	("C-M-l" . puni-slurp-forward)
	("C-M-b" . puni-barf-backward)
	("C-M-n" . puni-barf-forward)
	("C-M-p s" . puni-split)
	("C-M-p e" . puni-end-of-sexp)
	("C-M-p b" . puni-beginning-of-sexp)
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

  :bind
  ("C-c w" . transient-window-management))

;; (use-package tempel
;;   :bind (("C-c j j" . tempel-complete)
;; 	 ("C-c j i" . tempel-insert)
;; 	 ("C-c j h" . tempel-previous)
;; 	 ("~" . tempel-previous)
;; 	 ("S-<iso-lefttab>" . tempel-next))
;;   :commands (tempel-complete)
;;   :init
;;   (defun tempel-setup-capf ()
;;     (setq-local completion-at-point-functions
;; 		(cons #'tempel-expand
;; 		      completion-at-point-functions)))
;;   :hook ((org-mode prog-mode text-mode) . 'tempel-setup-capf))

;; ;; (use-package tempel-collection
;;   :load-path (expand-file-name "tony/tempel-collection" user-emacs-directory)
;;   :after tempel)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 5
	vertico-resize t
	vertico-cycle t))

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

(bind-keys :prefix-map t-map
	   :prefix "C-c"
	   ("o a" . org-agenda)
	   ;; magit
	   ("m m" . magit)
	   ;; ("C-c c" . with-editor-finish))
	   )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" default))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(meow projectile-ripgrep edit-indirect projectile flutter dart-mode exec-path-from-shell markdown-mode evil-org prescient magit vertico-prescient kaolin-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'magit-diff-edit-hunk-commit 'disabled nil)