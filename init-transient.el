(require 'transient)

(use-package transient
  :ensure t
  :config
  (setq transient-show-popup 0.15)
  (setq transient-detect-key-conflicts t)
  (setq transient-default-level 5)
  (transient-bind-q-to-quit)
  
  :bind
  ("C-c w" . transient-window-management)
  ("C-c e" . transient-search-replace-menu)
  ("C-c o" . transient-denote-menu)
  ("C-c g" . transient-magit-menu)
  ("C-c t" . transient-treesit-menu))

;; Magit transient
(transient-define-prefix transient-magit-menu ()
  "Magit menu"
  [["Branches"
    ("b" "Branch" magit-branch)
    ("c" "Checkout" magit-checkout)
    ("s" "Spin-off" magit-branch-spinoff)
    ("S" "Spin-out" magit-branch-spinout)]
   ["Changes"
    ("C" "Commit" magit-commit)
    ("P" "Push" magit-push)
    ("F" "Pull" magit-pull)]
   ["Other"
    ("l" "Log" magit-log)
    ("d" "Diff" magit-diff)]])

;; Editing utility transient
;;    puni, search, replace, etc.
(transient-define-prefix transient-search-replace-menu ()
  "Search and Replace menu"
  [["Search"
    ("o" "Occur" occur)]
   ["Replace"
    ("rs" "Search ripgrep" projectile-ripgrep)
    ("rr" "Replace" projectile-replace)]

   ["Edit"
    ("pk" "Kill paragraph" kill-paragraph)
    ("pm" "Mark paragraph" mark-paragraph)]

   ["Undo"
    ("uv" "vundo" vundo)]
   
   ["Grep"
    ("g" "Grep" grep)
    ("G" "Recursive grep" rgrep)]

   ["Mark"
    ("s" "Mark sexp at point" puni-mark-sexp-at-point)
    ("a" "Mark sexp around point" puni-mark-sexp-around-point)
    ("d" "Mark list around point" puni-mark-list-around-point)]
   
   ["Modify"
    ("x" "Squeeze" puni-squeeze)
    ("z" "Splice" puni-splice)]
   ["Slurp/Barf"
    ("b" "Slurp backward" puni-slurp-backward)
    ("f" "Slurp forward" puni-slurp-forward)
    ("}" "Barf backward" puni-barf-backward)
    ("{" "Barf forward" puni-barf-forward)]
   ["Other"
    ("j" "Split" puni-split)
    ("S" "Beginning of sexp" puni-beginning-of-sexp)
    ("e" "End of sexp" puni-end-of-sexp)
    ("c" "Wrap curly" puni-wrap-curly)
    ("(" "Wrap round" puni-wrap-round)
    ("X" "Force delete" puni-force-delete)]])

(transient-define-prefix transient-window-management ()
    "Window management menu"
    [["Navigation"
      ("n" "Next window" other-window)
      ("p" "Previous window" (lambda () (interactive) (other-window -1)))]
     ["Split"
      ("-" "Split below" split-window-below)
      ("/" "Split right" split-window-right)]
     ["Delete"
      ("q" "Delete window" delete-window)
      ("d" "Delete other windows" delete-other-windows)]
     ["Close Direction"
      ("ku" "Close Up" (lambda () (interactive) (close-all-windows-direction 'above)))
      ("kd" "Close Down" (lambda () (interactive) (close-all-windows-direction 'below)))
      ("kl" "Close Left" (lambda () (interactive) (close-all-windows-direction 'left)))
      ("kr" "Close Right" (lambda () (interactive) (close-all-windows-direction 'right)))]
     ["Resize"
      ("B" "Shrink horizontally" shrink-window-horizontally)
      ("F" "Enlarge horizontally" enlarge-window-horizontally)
      ("N" "Shrink vertically" shrink-window)
      ("P" "Enlarge vertically" enlarge-window)]
     ["Balance"
      ("=" "Balance windows" balance-windows)]
     ["Winner"
      ("u" "Undo" winner-undo)
      ("r" "Redo" winner-undo)]])

(transient-define-prefix transient-denote-menu ()
  "Denote menu"
  [["Files"
    ("o" "Open denote directory" open-denote-directory-in-projectile-dired)
    ("c" "Capture node" denote)
    ("s" "Search denote files" search-denote-directory)]])

(transient-define-prefix transient-treesit-menu ()
  "Tree-sitter folding menu"
  [["Folding"
    ("t" "Toggle fold" treesit-fold-toggle)
    ("o" "Open all folds" treesit-fold-open-all)
    ("c" "Close all folds" treesit-fold-close-all)]
   ["Indicators"
    ("i" "Toggle indicators" treesit-fold-indicators-mode)]])

(provide 'init-transient)
