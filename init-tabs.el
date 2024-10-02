


(use-package tab-bar
  :config
  (setq tab-bar-new-tab-choice 'directory)
  (keymap-set tab-prefix-map "e" '(lambda () (interactive) (find-file-other-tab (expand-file-name "init.el" user-emacs-directory))))
  (keymap-set tab-prefix-map "w" '(lambda () (interactive) (dired-other-tab "~/documents/nurts/writings")))
  (keymap-set tab-prefix-map "q" '(lambda () (interactive) (dired-other-tab "~/Developer"))))



(provide 'init-tabs)
