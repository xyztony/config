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

(provide 'init-corfu)
