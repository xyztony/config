(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package avy
  :bind
  (:map global-map
        ("C-c j c" . avy-goto-char)
        ("C-c j l" . avy-goto-line)
        ("C-c j m l" . avy-move-line)
        ("C-c j m r" . avy-move-region)))

(use-package dired
  :after dired
  :bind
  (:map dired-mode-map
	("C-c C-n" . dired-create-empty-file)
        ("C-c C-a" . gptel-add)))

(use-package wgrep
  :ensure t)

(use-package multiple-cursors
  :bind
  (:map global-map
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C->" . mc/mark-all-like-this)
        ("C-c C-s C->" . mc/skip-to-next-like-this)
        ("C-C C-C" . mc/edit-lines)
        ("C-c C-d" . mc/mark-next-like-this)))

(use-package popper
  :ensure t)
(require 'popper)
(popper-mode +1)
(require 'popper-echo)
(popper-echo-mode +1)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
	"\\*eshell\\*"
        "\\*Async Shell Command\\*"
	"\\*cider-repl*"
	"\\*cider-inspect\\*"
	"\\*xref\\*"
	"\\*ripgrep-search\\*"
        help-mode
        compilation-mode))
(global-set-key (kbd "C-`") 'popper-toggle)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "M-~") 'popper-cycle-backwards)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(global-set-key (kbd "C-M-k") 'popper-kill-latest-popup)

(defun ant/beginning-of-line ()
  "Swap between beginning of line, and first char of line."
  (interactive)
  (if (= 0 (current-column))
      (back-to-indentation)
    (beginning-of-line)))

(provide 'init-nav)
