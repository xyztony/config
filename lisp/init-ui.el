;; (add-hook 'after-init-hook #'dired-jump)
;; (add-hook 'after-change-major-mode-hook 'hack-local-variables)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#916668")

(winner-mode)
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

(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "Agave Nerd Font Mono" :height 160)
(set-face-attribute 'fixed-pitch nil :font "Agave Nerd Font Mono" :height 160)

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


(defun close-all-windows-direction (direction)
  "Close windows in the specified DIRECTION until no more windows exist"
  (let ((win (window-in-direction direction)))
    (when win
      (delete-window win)
      (close-all-windows-direction direction))))

(global-set-key (kbd "<f11>") 'toggle-frame-tab-bar)
(global-set-key (kbd "<f12>") 'toggle-frame-fullscreen)


(provide 'init-ui)
