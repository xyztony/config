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
    
  (defun ant/with-vertico-standard-mode (orig-fun &rest args)
    (let ((was-flat-mode-on (bound-and-true-p vertico-flat-mode)))
      (when was-flat-mode-on
        (vertico-flat-mode -1))
      (unwind-protect
          (apply orig-fun args)
        (when was-flat-mode-on
          (vertico-flat-mode 1)))))
  
  (advice-add 'affe-grep :around #'ant/with-vertico-standard-mode)
  (advice-add 'affe-find :around #'ant/with-vertico-standard-mode))

(provide 'init-vertico)
