;; inspired by https://github.com/alaq/workflowy.el/
(use-package org
  :hook
  ((org-mode . visual-line-mode))
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
   org-html-doctype "html5")
  (dolist (face '((org-level-1 . 1.4)
		  (org-level-2 . 1.2)
		  (org-level-3 . 1.1)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Agave Nerd Font Mono" :weight 'medium :height (cdr face))))

(defun my/screenshot-filename ()
  (let* ((buf-name (file-name-base (or (buffer-file-name) "screenshot")))
         (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S"))
         (filename (concat buf-name "_" timestamp ".png")))
    ;; Use this with org-attach-screenshot-command
    (setq org-attach-screenshot-command 
          (concat "screencapture -T5 " 
                  (expand-file-name filename org-attach-screenshot-dir)))))

(use-package org-attach-screenshot
  :ensure t
  :config
  (setq org-attach-screenshot-command-line "screencapture -T5 %f")
  (define-key org-mode-map (kbd "C-c s c") 'org-attach-screenshot))

;;;###autoload
(define-minor-mode org-zoom-mode
  "Zoom's features in an emacs minor mode for org-mode."
  nil nil nil
  (org-indent-mode)
  ;; (add-hook 'org-mode-hook 'set-olivetti)
  (add-hook 'org-mode-hook 'org-indent-mode)
  ;; (add-hook 'org-mode-hook #'org-bullet-mode)
  (define-key org-mode-map (kbd "C-<")
              'org-go-up-one-level)
  (define-key org-mode-map (kbd "C->")
              'org-go-down-one-level)
  (define-key org-mode-map (kbd "C-M-<") 'org-go-to-file-root)
  (define-key org-mode-map (kbd "C-M->") 'org-go-to-last-node)
  (define-key org-mode-map (kbd "C-c f") 'org-set-favorite-node)
  (define-key org-mode-map (kbd "C-c F") 'org-jump-to-favorite-node)
  (define-key org-mode-map (kbd "C-c C-f") 'org-clear-favorite-node)
  (define-key org-mode-map (kbd "C-c b") 'org-return-from-favorite)
  (setq header-line-format (concat "%b/" (org-format-outline-path(org-get-outline-path)))))

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
  (beginning-of-buffer)
  (widen)
  (setq header-line-format (concat "%b/" (org-format-outline-path (org-get-outline-path))))
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
  (setq header-line-format (concat "%b/" (org-format-outline-path (org-get-outline-path t)))))

(defun org-go-to-file-root ()
  "Go to the root level of the Org file while keeping the cursor on the current node."
  (interactive)
  (widen)
  (setq header-line-format (concat "%b/"))
  (recenter))

(defun org-go-to-last-node ()
  "Go to the last visited node."
  (interactive)
  (when org-node-history-ring
    (let ((marker (ring-remove org-node-history-ring 0)))
      (widen)
      (goto-char (marker-position marker))
      (org-narrow-to-subtree)
      (org-show-entry)
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
          (widen)
          (goto-char (marker-position target))
          (org-narrow-to-subtree)
          (org-show-entry)
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
