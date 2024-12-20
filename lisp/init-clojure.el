(require 'clj-refactor)

(use-package cider
  :ensure t
  :init
  :config
  (cider-enable-flex-completion)
  (setq cider-clojure-compilation-error-phases nil
	cider-repl-display-help-banner nil
	cider-stacktrace-fill-column 80
        cider-use-overlays t
        cider-show-error-buffer nil
        cider-auto-jump-to-error nil)
  :bind
  (:map cider-mode-map
	("C-x C-e" . cider-eval-dwim)
	("C-x C-r" . cider-pprint-eval-last-sexp-to-repl)
        ("C-c C-a" . cider-inspect-last-result)
	("C-c f d" . cider-find-dwim)
	("C-c f n" . cider-find-ns)
	("C-c f k" . cider-find-key))
  (:map cider-repl-mode-map
	("C-c M-l" . cider-repl-clear-buffer)
        ("C-c M-l" . cider-repl-clear-buffer)
	("C-c C-a" . cider-inspect-last-result)
        ("C-c X" . cider-selector)
        ("M-RET" . clerk-show)))

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clojure-mode
  :custom
  (setq clojure-indent-style 'always-indent
      clojure-indent-keyword-style 'always-indent
      clojure-enable-indent-specs nil)
  :hook
  ((clojure-mode . my-clojure-mode-hook))
  :bind
  (:map clojure-mode-map
        ("M-a" . clojure-backward-logical-sexp)
        ("M-e" . clojure-forward-logical-sexp)))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;; (defun add-debug-bindings ()
;;   "Create (def *var var) for all variables in let block and copy to kill-ring."
;;   (interactive)
;;   (save-excursion
;;     (let* ((bindings (cljr--get-let-bindings))
;;              (vars (mapcar 'car bindings))
;;              (defs (mapconcat (lambda (var)
;;                                 (format "(def *%s %s)" var var))
;;                               vars
;;                               "\n")))
;;         (kill-new defs)
;;         (message "Copied debug bindings to kill-ring"))))

(defun add-debug-bindings ()
  "Create (def *var var) for all variables in let block or defn args,
including destructured :as bindings, and copy to kill-ring."
  (interactive)
  (save-excursion
    (let (vars)
      (cond
       ((eq nil (cljr--goto-enclosing-sexp))
        (message "Inside a let binding at position: %d" (point))
        (let* ((bindings (cljr--get-let-bindings))
               (let-vars (mapcar #'car bindings)))
          (message "Let bindings: %S" bindings)
          (message "Let vars: %S" let-vars)
          (setq vars (append vars (add-debug--collect-vars let-vars)))))
       (t
        (message "Not inside a let binding or function definition.")))
      ;; Generate defs
      (when vars
        (let ((defs (mapconcat (lambda (var)
                                 (format "(def *%s %s)" var var))
                               vars
                               "\n")))
          (message "Generated defs:\n%s" defs)
          (kill-new defs)
          (message "Copied debug bindings to kill-ring"))))))


(defun add-debug--collect-vars (bindings)
  "Recursively collect variables from BINDINGS, including :as in destructured maps."
  (message "add-debug--collect-vars called with: %S" bindings)
  (cond
   ((symbolp bindings)
    (message "Found symbol: %s" bindings)
    (list bindings))
   ((listp bindings)
    (let (vars)
      (dolist (b bindings)
        (message "Processing binding: %S" b)
        (cond
         ((or (stringp b) (symbolp b))
          (message "Pushing symbol: %s" b)
          (push b vars))
         ((and (listp b) (keywordp (car b)))
          (message "Processing keyword map: %S" b)
          (let ((as-pos (cl-position :as b)))
            (when as-pos
              (let ((as-var (nth (1+ as-pos) b)))
                (when (or (symbolp as-var) (stringp as-var))
                  (let ((var-name (if (symbolp as-var)
                                      (symbol-name as-var)
                                    as-var)))
                    (message "Found :as variable: %s" var-name)
                    (push var-name vars)))))))
         (t
          (message "Recursing into: %S" b)
          (setq vars (append vars (add-debug--collect-vars b))))))
      (message "Vars collected so far: %S" vars)
      vars))
   (t
    (message "No symbols found in: %S" bindings)
    nil)))

;; (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

(provide 'init-clojure)

