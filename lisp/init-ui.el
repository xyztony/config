;; (add-hook 'after-init-hook #'dired-jump)
;; (add-hook 'after-change-major-mode-hook 'hack-local-variables)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#916668")

(winner-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)

(setq-default initial-buffer-choice "~/Developer")
(setq-default inhibit-startup-screen nil)
(setq-default initial-scratch-message nil)
(setq-default frame-resize-pixelwise t)
(setq-default startup-screen-inhibit-startup-screen 't)
(setq-default visible-bell nil)
(setq-default ring-bell-function 'ignore)
(setq-default display-line-numbers 'relative)
(setq-default highlight-nonselected-windows 't)

(setq my-font-size 140)

(setopt display-fill-column-indicator-column 120)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "Agave Nerd Font Mono" :height my-font-size)
(set-face-attribute 'fixed-pitch nil :font "Agave Nerd Font Mono" :height my-font-size)

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
 org-use-speed-commands t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; LSP
 gc-cons-threshold (* 100 1024 1024)
 read-process-output-max (* 1024 1024)
 company-minimum-prefix-length 1
 lsp-enable-indentation nil 
 lsp-enable-completion-at-point nil)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.4)
		(org-level-2 . 1.2)
		(org-level-3 . 1.1)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Agave Nerd Font Mono" :weight 'medium :height (cdr face)))


(defun my/close-all-windows-direction (direction)
  "Close windows in the specified DIRECTION until no more windows exist"
  (let ((win (window-in-direction direction)))
    (when win
      (delete-window win)
      (my/close-all-windows-direction direction))))

(defun my/split-and-show-other ()
  "Split window and show other buffer."
  (interactive)
  (let ((other-buf (other-buffer (current-buffer) t)))
    (if other-buf
        (progn
          (select-window (split-window-sensibly))
          (switch-to-buffer other-buf))
      (split-window-sensibly))))

(defun my/split-and-show-other (&optional split-direction)
  (interactive)
  (let* ((other-buf (other-buffer (current-buffer) t))
         (split-fn (cond ((eq split-direction 'horizontal) #'split-window-below)
                        ((eq split-direction 'vertical) #'split-window-right)
                        (t #'split-window-sensibly))))
    (when other-buf
      (select-window (funcall split-fn))
      (switch-to-buffer other-buf))))

(global-set-key (kbd "C-x 4") (lambda () (interactive) (my/split-and-show-other nil)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (my/split-and-show-other 'vertical)))
(global-set-key (kbd "C-x 2") (lambda () (interactive) (my/split-and-show-other 'horizontal)))

(global-set-key (kbd "<f11>") 'toggle-frame-tab-bar)
(global-set-key (kbd "<f12>") 'toggle-frame-fullscreen)

(provide 'init-ui)
