(use-package symbol-overlay
  :custom-face
  (symbol-overlay-face-1 ((t (:background "dodger blue" :foreground "black"))))
  (symbol-overlay-face-2 ((t (:background "hot pink" :foreground "black"))))
  (symbol-overlay-face-3 ((t (:background "yellow" :foreground "black"))))
  (symbol-overlay-face-4 ((t (:background "orchid" :foreground "black"))))
  (symbol-overlay-face-5 ((t (:background "red" :foreground "black"))))
  (symbol-overlay-face-6 ((t (:background "salmon" :foreground "black"))))
  (symbol-overlay-face-7 ((t (:background "spring green" :foreground "black"))))
  (symbol-overlay-face-8 ((t (:background "turquoise" :foreground "black"))))
  :bind (("M-o" . nil)
         ("M-o o". symbol-overlay-put)
	 ("M-o f" . symbol-overlay-jump-next)
         ("M-o M-o". symbol-overlay-put)
         ("M-o r" . symbol-overlay-remove-all)
         ("M-o M-r" . symbol-overlay-remove-all)         
         ("M-o s" . symbol-overlay-toggle-in-scope)
         ("M-o M-s" . symbol-overlay-toggle-in-scope)))

(provide 'init-symbol)
