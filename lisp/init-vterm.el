(use-package vterm
  :defer t
  :commands (vterm--internal)
  :config
  (with-eval-after-load 'vterm
    (setq vterm-kill-buffer-on-exit nil)
    (advice-add 'vterm :after
                (lambda (buf)
                  (with-current-buffer buf
                    (set-process-query-on-exit-flag
                     (get-buffer-process (current-buffer)) nil)))))
  (defun new-vterm ()
    (interactive)
    (vterm--internal #'pop-to-buffer-same-window (gensym)))
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode 0)))
  :bind
  (:map global-map
        ("C-c n t" . new-vterm)))

(provide 'init-vterm)
