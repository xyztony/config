(use-package with-editor
  :ensure t
  :vc (:url "https://github.com/magit/with-editor"
            :rev :newest))

(use-package magit
  :ensure t
  :init
  (require 'with-editor)
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-magit)
