(use-package treesit
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.jl\\'" . julia-ts-mode)
	 ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.pl\\'" . prolog-ts-mode))
  :hook ((clojure-ts-mode . (lambda () (clojure-mode) (cider-mode)))
	 (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp))))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
               (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src"))
               (prolog . ("https://github.com/foxyseta/tree-sitter-prolog"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (mapping
           '((clojure-mode . clojure-ts-mode)
	     (clojure-script-mode . clojure-ts-mode)
             (css-mode . css-ts-mode)
	     (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (prolog-mode . prolog-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(defun treesit-get-containing-list (node)
  "Get the containing list_lit node starting from NODE."
  (if (equal (treesit-node-type node) "list_lit")
      node
    (let ((parent (treesit-node-parent node)))
      (when parent
        (if (equal (treesit-node-type parent) "list_lit")
            parent
          (treesit-get-containing-list parent))))))

(defun treesit-clojure-let-binding-names ()
  "Return a list of binding names from the Clojure let form at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
        (list-node (treesit-get-containing-list node))
        (first-child (when list-node
                      (treesit-node-child list-node 1)))
        (is-let (and first-child
                     (equal (treesit-node-type first-child) "sym_lit")
                     (equal (treesit-node-text first-child) "let")))
        (bindings-vec (when is-let
                       (treesit-node-child list-node 2)))
        (binding-names
         (when (and bindings-vec
                    (equal (treesit-node-type bindings-vec) "vec_lit"))
           (let (names)
             (dolist (child (treesit-node-children bindings-vec))
               (when (equal (treesit-node-type child) "sym_lit")
                 (push (treesit-node-text child) names)))
             (nreverse names)))))
    (when binding-names
      (let ((names-string (mapconcat #'identity binding-names " ")))
        (kill-new names-string)
        (message "Copied to kill-ring: [%s]" names-string)))))

(defun treesit-list-clojure-functions ()
  "List all function definitions (defn) in current buffer or form.
Results are displayed in a new buffer."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (query-string "(list_lit
                         (sym_lit) @defn-sym
                         (sym_lit) @fn-name
                         [
                          (vec_lit) @args
                          (list_lit) @body
                         ]
                         (#equal @defn-sym \"defn\"))")
         (query (treesit-query-compile 'clojure query-string))
         (root-node (treesit-buffer-root-node 'clojure))
         (matches (treesit-query-capture root-node query))
         
         (functions (seq-uniq
                     (seq-filter (lambda (match)
                                   (equal (car match) 'fn-name))
                                 matches)
                     (lambda (a b)
                       (equal (treesit-node-text (cdr a))
                              (treesit-node-text (cdr b))))))
         
         (result-buffer (get-buffer-create "*Clojure Functions*"))
         
         (markers (mapcar (lambda (fn)
                            (let* ((node (cdr fn))
                                   (pos (treesit-node-start node))
                                   (marker (make-marker)))
                              (with-current-buffer source-buffer
                                (set-marker marker pos))
                              (cons (treesit-node-text node) marker)))
                          functions)))
    (with-current-buffer result-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (item markers)
          (let ((name (car item))
                (marker (cdr item)))
            (insert-text-button 
             name
             'action (lambda (btn)
                       (let ((marker-pos (button-get btn 'marker-pos))
                             (marker-buffer (button-get btn 'marker-buffer)))
                         (when (and marker-pos
                                    marker-buffer
                                    (buffer-live-p marker-buffer))
                           (pop-to-buffer marker-buffer)
                           (goto-char marker-pos))))
             'marker-pos (marker-position marker)
             'marker-buffer (marker-buffer marker)
             'follow-link t
             'help-echo "Go to function definition")
            (insert "\n")))
        (goto-char (point-min))
        (special-mode)))
    
    (display-buffer result-buffer)
    (when (fboundp 'popper-toggle-type)
      (popper-toggle-type result-buffer))))

(use-package treesit-fold
  :load-path "treesit-fold")

(use-package treesit-fold-indicators
   :load-path "treesit-fold")

(provide 'init-treesit)
