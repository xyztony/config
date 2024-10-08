(use-package elec-pair
  :config
  (electric-pair-mode +1)
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
			      (?\< . ?\>)
                              (?\( . ?\))
                              (?\[ . ?\]))))

(use-package puni
  :defer t
  :init (puni-global-mode)
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  (term-mode-hook . puni-disable-puni-mode)
  :bind
  (:map puni-mode-map
	("C-M-s" . puni-mark-sexp-at-point)
	("C-M-a" . puni-mark-sexp-around-point)
	("C-M-d" . puni-mark-list-around-point)

	("C-M-x" . puni-squeeze)
	("C-M-z" . puni-splice)

	("C-M-b" . puni-slurp-backward)
	("C-M-f" . puni-slurp-forward)

	("C-M-}" . puni-barf-backward)
	("C-M-{" . puni-barf-forward)))

(provide 'init-lisp-stuff)
