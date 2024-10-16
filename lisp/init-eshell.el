(require 'popper)
(require 'popper-echo)
(popper-echo-mode +1)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
	"*eshell*"
        "\\*Async Shell Command\\*"
	"*cider-repl\\*$"
        help-mode
        compilation-mode))
(global-set-key (kbd "C-`") 'popper-toggle)  
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(global-set-key (kbd "C-M-k") 'popper-kill-latest-popup)
(popper-mode +1)

(defvar eshell-up-ignore-case t "Non-nil if searches must ignore case.")

(defvar eshell-up-print-parent-dir nil "Non-nil if the parent directory must be printed before ‘eshell-up’ changes to it.")

(defun eshell-up-closest-parent-dir (file)
  "Find the closest parent directory of a file.
Argument FILE the file to find the closest parent directory for."
  (file-name-directory
   (directory-file-name
    (expand-file-name file))))

(defun eshell-up-find-parent-dir (path &optional match)
  "Find the parent directory based on the user's input.
Argument PATH the source directory to search from.
Argument MATCH a string that identifies the parent directory to search for."
  (let ((closest-parent (eshell-up-closest-parent-dir path)))
    (if match
        (let ((case-fold-search eshell-up-ignore-case))
          (locate-dominating-file closest-parent
                                  (lambda (parent)
                                    (let ((dir (file-name-nondirectory
                                                (expand-file-name
                                                 (directory-file-name parent)))))
                                      (if (string-match match dir)
                                          dir
                                        nil)))))
      closest-parent)))

;;;###autoload
(defun eshell-up (&optional match)
  "Go to a specific parent directory in eshell.
Argument MATCH a string that identifies the parent directory to go
to."
  (interactive)
  (let* ((path default-directory)
         (parent-dir (eshell-up-find-parent-dir path match)))
    (progn
      (when parent-dir
        (eshell/cd parent-dir))
      (when eshell-up-print-parent-dir
        (if parent-dir
            (eshell/echo parent-dir)
          (eshell/echo path))))))

;;;###autoload
(defun eshell-up-peek (&optional match)
  "Find a specific parent directory in eshell.
Argument MATCH a string that identifies the parent directory to find"
  (interactive)
  (let* ((path default-directory)
         (parent-dir (eshell-up-find-parent-dir path match)))
    (if parent-dir
        parent-dir
      path)))

(use-package eshell
  :config
  (setq eshell-banner-message "")
  :bind
  (:map eshell-mode-map
	("C-c f" . eshell-up)))

(provide 'init-eshell)
