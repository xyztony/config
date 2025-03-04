(use-package vterm
  :defer t
  :config
  (with-eval-after-load 'vterm
    (setq vterm-kill-buffer-on-exit nil)
    (advice-add 'vterm :after
                (lambda (buf)
                  (with-current-buffer buf
                    (set-process-query-on-exit-flag
                     (get-buffer-process (current-buffer)) nil))))))

(provide 'init-vterm)
