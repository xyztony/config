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

(defun my/mc-mark-word-and-similar ()
    "Mark word at point and similar words after it."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (when word
        (mc/mark-all-words-like-this))))

(use-package multiple-cursors
  :bind
  (:map global-map
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)
        ("C-C C-C" . mc/edit-lines)
        ("C-c C-d" . mc/mark-next-like-this)
        ("C-c C-w" . my/mc-mark-word-and-similar)))

(defun my/beginning-of-line ()
  "Swap between beginning of line, and first char of line."
  (interactive)
  (if (= 0 (current-column))
      (back-to-indentation)
    (beginning-of-line)))

(provide 'init-nav)
