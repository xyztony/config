(use-package popper
  :ensure t)
(require 'popper)
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
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(global-set-key (kbd "C-M-k") 'popper-kill-latest-popup)
(popper-mode +1)

(setq eshell-banner-message "")

(provide 'init-eshell)
