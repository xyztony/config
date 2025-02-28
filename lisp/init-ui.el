;; (add-hook 'after-init-hook #'dired-jump)
;; (add-hook 'after-change-major-mode-hook 'hack-local-variables)

(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#916668")

(winner-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(pixel-scroll-precision-mode 1)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq-default initial-buffer-choice "~/Developer")
(setq-default inhibit-startup-screen nil)
(setq-default initial-scratch-message nil)
(setq-default frame-resize-pixelwise t)
(setq-default startup-screen-inhibit-startup-screen 't)
(setq-default visible-bell nil)
(setq-default ring-bell-function 'ignore)
(setq-default display-line-numbers 'relative)
(setq-default highlight-nonselected-windows 't)

(defun ant/set-fonts (font-size)
  (set-face-attribute 'default nil
                        :font "Agave Nerd Font Mono"
                        :height font-size
                        :weight 'normal)
    (set-face-attribute 'fixed-pitch nil
                        :font "Agave Nerd Font Mono"
                        :height font-size
                        :weight 'light))

(defun ant/set-font-size ()
  (interactive)
  (let ((size
         (* 10 (string-to-number
                (completing-read "font size >> "
                                 (mapcar #'number-to-string
                                         (number-sequence 10 30)))))))
    (ant/set-fonts size)))

(defconst my-font-size 180)
(ant/set-fonts my-font-size)

(setopt display-fill-column-indicator-column 120)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(load-theme 'modus-vivendi-tritanopia t)
(use-package almost-mono-themes
  :ensure t)
(use-package humanoid-themes
  :ensure t)
;; (load-theme 'humanoid-dark t)
;; (load-theme 'almost-mono-cream)

(use-package moody
  :vc (:url "https://github.com/tarsius/moody"
            :rev :newest)
  :config
  (setq moody-mode-line-height 20
        moody-slant-function #'moody-slant-apple-rgb)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
  
(require 'org-faces)

(dolist (face '(window-divider
		window-divider-first-pixel
		window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(defun ant/close-all-windows-direction (direction)
  "Close windows in the specified DIRECTION until no more windows exist"
  (let ((win (window-in-direction direction)))
    (when win
      (delete-window win)
      (ant/close-all-windows-direction direction))))

(defun ant/split-and-show-other (&optional split-direction)
  (interactive)
  (let* ((other-buf (other-buffer (current-buffer) t))
         (split-fn (cond ((eq split-direction 'horizontal) #'split-window-below)
                        ((eq split-direction 'vertical) #'split-window-right)
                        (t #'split-window-sensibly))))
    (when other-buf
      (select-window (funcall split-fn))
      (switch-to-buffer other-buf))))

(global-set-key (kbd "C-x 3") (lambda () (interactive) (ant/split-and-show-other 'vertical)))
(global-set-key (kbd "C-x 2") (lambda () (interactive) (ant/split-and-show-other 'horizontal)))

(global-set-key (kbd "<f11>") 'toggle-frame-tab-bar)
(global-set-key (kbd "<f12>") 'toggle-frame-fullscreen)

(provide 'init-ui)
