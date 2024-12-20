(defun my-remove-tracking ()
  (interactive)
  (when-let ((url (eww-current-url)))
    (eww (eww-remove-tracking url))))

(use-package eww
  :config
  (setq eww-search-prefix (concat (getenv "KAGI_BASE") "&q=")
        shr-use-fonts nil
        shr-width 90)
  :bind
  (
   :map eww-mode-map
   ("v" . nil) ; override `eww-view-source' idc
   ("O" . eww-browse-with-external-browser)
   ("t" . my-remove-tracking)
   :map eww-buffers-mode-map
   ("d" . eww-bookmark-kill)
   :map eww-bookmark-mode-map
   ("d" . eww-bookmark-kill)))

(provide 'init-eww)
