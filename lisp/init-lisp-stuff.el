(use-package elec-pair
  :config
  (electric-pair-mode +1)
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
			      ;; (?\< . ?\>)
                              ;; (?\\\* . ?\\*\\)
                              (?\( . ?\))
                              (?\[ . ?\]))))

(use-package puni
  :defer t
  :init (puni-global-mode)
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  (term-mode-hook . puni-disable-puni-mode)
  :config
  (defun my/puni-raise-and-replace-sexp ()
    "Raise sexp at point, entirely replacing wherever it was previously contained"
    (interactive)
    (puni-mark-sexp-around-point)
    (let ((sexp (buffer-substring-no-properties (region-beginning) (region-end))))
      (puni-mark-sexp-around-point)
      (delete-region (region-beginning) (region-end))
      (insert sexp)))

  (defun my/duplicate-region-after ()
    "Duplicate the current region, insert on a new line after."
    (interactive)
    (save-excursion
      (let* ((start (region-beginning))
             (end (region-end))
             (sexp-str (buffer-substring-no-properties start end)))
        (goto-char end)
        (newline-and-indent)
        (insert sexp-str))))

  (defun my/puni-grab-next-sexp-here ()
    "Grab the next s-expression and bring it to current position with newline."
    (interactive)
    (let ((orig-point (point)))
      (puni-forward-sexp)
      
      (let ((sexp-end (point)))
        (puni-backward-sexp)
        (let ((sexp-text (buffer-substring-no-properties (point) sexp-end)))
          (delete-region (point) sexp-end)
          (goto-char orig-point)
          (kill-line)
          (insert sexp-text)
          (goto-char orig-point)
          (newline-and-indent)))))

  (defun my/puni-mark-and-indent-sexp ()
    "Mark sexp around point using puni and indent the region."
    (interactive)
    (let ((orig-point (point)))
      (puni-mark-sexp-around-point)
      (indent-region (region-beginning) (region-end))
      (goto-char orig-point)))
  
  :bind
  (:map puni-mode-map
	("C-M-s" . puni-mark-sexp-at-point)
        ("C-M-a" . puni-mark-sexp-around-point)
        
        ("C-M-x" . puni-squeeze)
	("C-M-z" . puni-splice)

        ("C-M-d" . my/duplicate-region-after)
        ("C-M-e" . my/puni-mark-and-indent-sexp)
        ("C-M-r" . my/puni-raise-and-replace-sexp)
        ("C-M-g" . my/puni-grab-next-sexp-here)
	
        ("C-M-b" . puni-slurp-backward)
	("C-M-f" . puni-slurp-forward)

	("C-M-}" . puni-barf-backward)
	("C-M-{" . puni-barf-forward)

        ("C-=" . puni-expand-region)))

(provide 'init-lisp-stuff)
