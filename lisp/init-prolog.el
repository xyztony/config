(require 'prolog)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi
      prolog-electric-if-then-else-flag t
      prolog-program-switches '((swi ("-G128M" "-T128M" "-L128M" "-O"))
                                (t nil)))

(defun insert-use-module ()
  "Inserts a :- use_module directive"
  (interactive)
  (goto-char (point-min))
  (open-line 1)
  (insert ":- use_module(library()).")
  (forward-char -3))

(defun quick-comment ()
  (interactive)
  (comment-indent)
  (insert " "))

(defun prolog-insert-comment-block ()
  "Insert a PceEmacs-style comment block like /* - - ... - - */ "
  (interactive)
  (let ((dashes "-"))
    (dotimes (_ 70) (setq dashes (concat "-" dashes)))
    (insert (format "/* %s\n\n%s */" dashes dashes))
    (forward-line -1)
    (indent-for-tab-command)))

(defun insert-query ()
  "Insert a ?- query with a period"
  (interactive)
  (insert "?- .")
  (forward-char -1))

(defun insert-prog ()
  "Insert a :- query with a period"
  (interactive)
  (insert ":- .")
  (forward-char -1))

(defun insert-eq ()
  (interactive)
  (insert "=:="))

(defun add-comma-eol ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun add-period-eol ()
  (interactive)
  (save-excursion 
    (end-of-line)
    (insert ".")))

(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

(use-package ediprolog
  :ensure t
  :config
  (setq ediprolog-program "scryer-prolog")

  :bind
  (:map prolog-mode-map
        ("C-c C-a" . ediprolog-remove-interactions)
        ("C-c C-b" . prolog-insert-comment-block)
        ("C-c C-c" . quick-comment)
        ("C-c C-e" . ediprolog-dwim)
        
        ("C-c C-l" . insert-use-module)
        ("C-c C-q" . insert-query)
        ("C-c C-p" . insert-prog)
        ("C-c C-<return>" . insert-eq)
        
        ("C-c ." . add-period-eol)
        ("C-c ," . add-comma-eol)
        ("C-c C-;" . comment-line)))

(provide 'init-prolog)
