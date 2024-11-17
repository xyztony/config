(defun my/corfu-eshell-mode-hook ()
  (lambda ()
    (setq-local corfu-auto nil)
    (corfu-mode)))

(use-package prescient
  :ensure t
  :config
  (setq prescient-sort-full-matches-first 't
        prescient-filter-method '(fuzzy regexp)
	completion-styles '(prescient basic)))

(use-package corfu-prescient
  :ensure t
  :after corfu)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-preselect-first t)
  (corfu-separator ?\s)
  (completion-category-overrides '((file (styles partial-completion))))
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (setq global-corfu-minibuffer
      (lambda ()
        (not (or (bound-and-true-p vertico--input)
                 (eq (current-local-map) read-passwd-map)))))
  :init
  (global-corfu-mode)
  (corfu-prescient-mode)
  
  :hook
  ((after-init . corfu-mode)
   (cider-repl-mode . corfu-mode)
   (cider-mode . corfu-mode)
   (eshell-mode . my/corfu-eshell-mode-hook))
  
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous)))


(provide 'init-corfu)
