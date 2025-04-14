(use-package prettier-js
  :defer t
  :custom
  (prettier-js-show-errors nil)
  :hook
  ((css-mode
    css-ts-mode
    markdown-mode
    scss-mode
    tsx-ts-mode
    typescript-mode
    typescript-ts-mode) . prettier-js-mode))

(provide 'init-webstuff)
