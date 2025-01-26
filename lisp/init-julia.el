(use-package julia-mode
  :ensure t)

(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))

(use-package julia-ts-mode
  :ensure t
  :mode "\\.jl$")

(provide 'init-julia)
