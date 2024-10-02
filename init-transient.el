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
  ("C-c s" . transient-search-replace-menu)
  ("C-c o" . transient-org-roam-menu)
  ("C-c g" . transient-magit-menu))

;; Magit transient
(transient-define-prefix transient-magit-menu ()
  "Magit menu"
  [["Status"
    ("s" "Status" magit-status)
    ("g" "Magit" magit)]])

;; Search and Replace transient
  (transient-define-prefix transient-search-replace-menu ()
    "Search and Replace menu"
    [["Search"
      ("s" "Search forward" isearch-forward)
      ("r" "Search backward" isearch-backward)
      ("o" "Occur" occur)]
     ["Replace"
      ("%" "Query replace" query-replace)
      ("M-%" "Query replace regexp" query-replace-regexp)]
     ["Edit"
      ("p" "Kill paragraph" kill-paragraph)]
     ["Grep"
      ("g" "Grep" grep)
      ("G" "Recursive grep" rgrep)]])


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

(transient-define-prefix transient-org-roam-menu ()
    "Org roam menu"
    [["Files"
      ("f" "Find file" org-roam-node-find)
      ("c" "Capture node" org-roam-capture)
      ("tt" "Capture today" org-roam-dailies-capture-today)
      ("tm" "Capture tomorrow" org-roam-dailies-capture-tomorrow)]])


(provide 'init-transient)
