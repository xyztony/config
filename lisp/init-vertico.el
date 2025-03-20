(require 'init-utils)

(use-package vertico
  :ensure t
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

(use-package affe
  :vc (:url "https://github.com/minad/affe" :rev :newest)
  :config
  (consult-customize affe-grep :preview-key "M-.")
  (advice-add 'affe-grep :around #'ant/with-vertico-mode)
  (advice-add 'affe-find :around #'ant/with-vertico-mode))

(advice-add 'consult-ripgrep :around #'ant/with-vertico-mode)

(provide 'init-vertico)
