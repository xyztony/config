;;; init-org.el  -*- lexical-binding: t; -*-
(use-package org
  :hook
  ((org-mode . (lambda ()
                 (ant/electric-pair-disable-pairs!)
                 (visual-line-mode))))

  :preface
  (defun ant/org-babel-create-sql-block ()
    "Create a new SQL source block with interactive prompts."
    (interactive)
    (let* ((sql-type (completing-read "SQL Type: " '("sqlite" "postgresql" "mysql") nil t))
           (query (read-string "Enter SQL query: "))
           (header-args (cond
                         ((string= sql-type "sqlite") ":results table :colnames yes")
                         ((string= sql-type "postgresql") ":results table :colnames yes :engine postgresql")
                         ((string= sql-type "mysql") ":results table :colnames yes :engine mysql")
                         (t ""))))
      (org-insert-structure-template "src")
      (save-excursion
        (end-of-line)
        (insert sql-type)
        (when (not (string-empty-p header-args))
          (insert " " header-args))
        (newline)
        (insert query))))
  
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-use-speed-commands t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   org-return-follows-link t
   org-src-window-setup 'current-window
   org-time-stamp-custom-formats (cons "<%Y-%m-%d>" "<%Y-%m-%d %a %H:%M>")
   org-time-stamp-formats (cons "<%Y-%m-%d %H:%M>" "<%Y-%m-%d>")
   org-html-doctype "html5"
   org-tag-alist '(("~pending~" . ?p)
                   ("~research" . ?R)
                   ("~stash" .    ?S)
                   ("~clj" .      ?c)
                   ("!go" .       ?G))
   org-agenda-files '("~/Documents/notes/")
   org-todo-keywords '((sequence "TODO" "WIP" "BLOCKED" "CANCELLED" "TRANSIENT" "DONE")))

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))

  (setq
   org-babel-default-header-args:sql 
   '((:column . t)
     (:header . t))

   org-babel-default-header-args:sqlite
   '((:column . t)
     (:header . t))
   
   org-confirm-babel-evaluate nil)
  
  ;; org-agenda
  (if (work-machine-p)
      (setq
       org-tag-alist
       (append org-tag-alist 
               '(("@reports" .  ?r)
                 ("@dash" .     ?d)))
       org-agenda-custom-commands
       '(("w" "Work"
          ((tags-todo ".*" ((org-agenda-files '("~/Documents/notes/20250101T000000--work-inbox.org"))
                            (org-agenda-overriding-header "Current Tasks")))))))
    (setq
     org-tag-alist
     ' (
        ("@reports" .  ?r)
        ("@dash" .     ?d)
        ("@pres" .     ?p)
        ("~pending~" . ?p)
        ("~research" . ?R)
        ("~stash" .    ?S))
     org-agenda-custom-commands
     '(("w" "Work"
        ((tags-todo ".*" ((org-agenda-files '("~/Documents/notes/20250101T000000--inbox.org"))
                          (org-agenda-overriding-header "Current Tasks"))))))))
  (dolist (face '((org-level-1 . 1.4)
		  (org-level-2 . 1.2)
		  (org-level-3 . 1.1)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Lilex" :weight 'medium :height (cdr face)))
  :bind
  (:map org-mode-map
        ("C-c s s" . ant/org-babel-create-sql-block)))


(use-package org-modern
  :vc (:url "https://github.com/minad/org-modern" :rev "main")
  :config
  (setq
   org-auto-align-tags nil
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-pretty-entities t)
  :hook
  (org-mode . org-modern-mode))

(use-package org-attach-screenshot
  :ensure t
  :config
  (setq org-attach-screenshot-command-line "screencapture -T5 %f")
  (define-key org-mode-map (kbd "C-c s c") 'org-attach-screenshot))

(defun ant/electric-pair-disable-pairs! ()
  "Disable ?<, ?{ in `org-mode' when using `electric-pair-mode'."
  (when (and electric-pair-mode (eql major-mode #'org-mode))
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (or (char-equal c ?<) (char-equal c ?{))
                       t
                     (,electric-pair-inhibit-predicate c))))))

;; inspired by https://github.com/alaq/workflowy.el/
(require 'ring)

(defun format-outline-header-path (suffix)
  (concat "%b/" (org-format-outline-path suffix)))

;;;###autoload
(define-minor-mode org-zoom-mode
  "Zoom's features in an emacs minor mode for org-mode."
  nil nil nil
  (org-indent-mode)
  ;; (add-hook 'org-mode-hook 'set-olivetti)
  (add-hook 'org-mode-hook 'org-indent-mode)
  ;; (add-hook 'org-mode-hook #'org-bullet-mode)
  (define-key org-mode-map (kbd "C-<")     'org-go-up-one-level)
  (define-key org-mode-map (kbd "C->")     'org-go-down-one-level)
  (define-key org-mode-map (kbd "C-M-<")   'org-go-to-file-root)
  (define-key org-mode-map (kbd "C-M->")   'org-go-to-last-node)
  (define-key org-mode-map (kbd "C-c f")   'org-set-favorite-node)
  (define-key org-mode-map (kbd "C-c C-f")   'org-jump-to-favorite-node)
  (define-key org-mode-map (kbd "C-c F") 'org-clear-favorite-node)
  (define-key org-mode-map (kbd "C-c b")   'org-return-from-favorite)
  (setq-local header-line-format
              (format-outline-header-path (org-get-outline-path))))

(defvar org-node-history-ring nil
  "Ring buffer storing visited node positions.")

(defvar org-node-history-size 50
  "Maximum number of nodes to remember in history.")

(defun org-remember-node-position ()
  "Store current node position in history ring."
  (let ((entry (point-marker)))
    (unless org-node-history-ring
      (setq org-node-history-ring (make-ring org-node-history-size)))
    (ring-insert org-node-history-ring entry)))

(defun org-clear-node-history ()
  "Clear the node history ring buffer."
  (interactive)
  (setq org-node-history-ring nil))

(defun org-go-up-one-level ()
  "Navigate to parent node, maintaining zoom view."
  (interactive)
  (goto-char (point-min))
  (widen)
  (setq header-line-format (format-outline-header-path (org-get-outline-path)))
  (save-excursion
    (org-up-element)
    (org-narrow-to-subtree))
  (recenter))

(defun org-go-down-one-level ()
  "Navigate into current node, maintaining zoom view."
  (interactive)
  (org-remember-node-position)
  (org-narrow-to-subtree)
  (org-fold-show-children)
  (setq header-line-format (format-outline-header-path (org-get-outline-path t))))

(defun org-go-to-file-root ()
  "Go to the root level of the Org file while keeping the cursor on the
current node."
  (interactive)
  (widen)
  (setq header-line-format "%b")
  (recenter))

(defun org-go-to-last-node ()
  "Go to the last visited node."
  (interactive)
  (when org-node-history-ring
    (let ((marker (ring-remove org-node-history-ring 0)))
      (widen)
      (goto-char (marker-position marker))
      (org-narrow-to-subtree)
      (org-fold-show-entry)
      (recenter))))

(defvar org-global-favorite-node nil
  "Marker for globally favorite org node position.")

(defvar-local org-buffer-favorite-node nil
  "Buffer-local marker for favorite org node position.")

(defvar org-previous-buffer-position nil
  "Store previous buffer and position before jumping to global favorite.")

(defun org-set-favorite-node (arg)
  "Set current node as favorite.
With universal prefix (C-u), set as global favorite.
Otherwise, set as buffer-local favorite."
  (interactive "P")
  (let ((marker (point-marker)))
    (if arg
        (progn
          (setq org-global-favorite-node marker)
          (message "Set global favorite node"))
      (setq org-buffer-favorite-node marker)
      (message "Set buffer-local favorite node"))))

(defun org-clear-favorite-node (arg)
  "Clear favorite node.
With universal prefix (C-u), clear global favorite.
Otherwise, clear buffer-local favorite."
  (interactive "P")
  (if arg
      (progn
        (setq org-global-favorite-node nil)
        (message "Cleared global favorite node"))
    (setq org-buffer-favorite-node nil)
    (message "Cleared buffer-local favorite node")))

(defun org-jump-to-favorite-node (arg)
  "Jump to favorite node.
With universal prefix (C-u), jump to global favorite.
Otherwise, jump to buffer-local favorite."
  (interactive "P")
  (let* ((target (if arg org-global-favorite-node org-buffer-favorite-node))
         (target-buffer (and target (marker-buffer target))))
    (if target
        (progn
          (when (and arg target-buffer)
            (push-mark)
            (setq org-previous-buffer-position 
                  (cons (current-buffer) (point-marker)))
            (switch-to-buffer target-buffer))
          (setq header-line-format (format-outline-header-path (org-get-outline-path t)))
          (widen)
          (goto-char (marker-position target))
          (org-narrow-to-subtree)
          (org-fold-show-entry)
          (recenter))
      (message "No %s favorite node set" (if arg "global" "buffer-local")))))

(defun org-return-from-favorite ()
  "Return to position before jumping to global favorite node."
  (interactive)
  (when org-previous-buffer-position
    (let ((prev-buffer (car org-previous-buffer-position))
          (prev-position (cdr org-previous-buffer-position)))
      (switch-to-buffer prev-buffer)
      (goto-char (marker-position prev-position))
      (setq org-previous-buffer-position nil))))

(provide 'init-org)
