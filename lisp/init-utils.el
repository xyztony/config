(defun ant/with-vertico-standard-mode (orig-fun &rest args)
    (let ((was-flat-mode-on (bound-and-true-p vertico-flat-mode)))
      (when was-flat-mode-on
        (vertico-flat-mode -1))
      (unwind-protect
          (apply orig-fun args)
        (when was-flat-mode-on
          (vertico-flat-mode 1)))))

(provide 'init-utils)
