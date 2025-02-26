(defcustom my/gptel-directives-file "gptel-custom/gptel-directives.org"
  "Path to the gptel directives org file, relative to =user-emacs-directory'."
  :type 'string
  :group 'gptel)

(use-package gptel
  :load-path "gptel"
  
  :custom
  (gptel-model 'claude-3-7-sonnet-20250219)
  
  :config
  (defun my/gptel-save-buffer ()
    "Save the current GPTEL buffer with the default directory
set to ~/Documents/notes."
    (interactive)
    (let ((default-directory "~/Documents/notes/gptel/"))
      (call-interactively #'save-buffer)))

  (defun my/gptel-load-session ()
    "Load a gptel session from ~/Documents/notes directory."
    (interactive)
    (let ((default-directory "~/Documents/notes/gptel/"))
      (let* ((files (directory-files default-directory t ".+\\.org$"))
             (file (completing-read "Select session file: " files nil t)))
        (when file
          (find-file file)
          (gptel-mode)))))

  (defun my/load-gptel-directives-from-org ()
    "Load gptel directives from an org file and merge with existing directives.
The org file should have level 1 headings as directive names
and their content as the directive text."
    (interactive)
    (let* ((directives-file (expand-file-name my/gptel-directives-file user-emacs-directory))
           (directives-table (make-hash-table :test 'eq)))
      
      ;; Load existing directives into the hash table
      (dolist (dir gptel-directives)
        (puthash (car dir) (cdr dir) directives-table))
      
      ;; Parse and merge new directives
      (with-temp-buffer
        (insert-file-contents directives-file)
        (org-mode)
        (goto-char (point-min))
        (let ((new-count 0))
          (while (re-search-forward "^\\* \\(.+\\)" nil t)
            (let* ((directive-name (intern (match-string-no-properties 1)))
                   (content-start (progn (forward-line) (point)))
                   (content-end (save-excursion
                                  (or (re-search-forward "^\\* " nil t)
                                      (point-max))))
                   (directive-content
                    (string-trim
                     (buffer-substring-no-properties content-start content-end))))
              (cl-incf new-count)
              (puthash directive-name directive-content directives-table)))
          
          ;; Convert hash table back to alist
          (setq gptel-directives
                (nreverse
                 (cl-loop for k being the hash-keys of directives-table
                          using (hash-values v)
                          collect (cons k v))))
          
          (message "Loaded %d new gptel directives (%d total)"
                   new-count (hash-table-count directives-table))))))
  
  (my/load-gptel-directives-from-org)

  ;; https://github.com/karthink/gptel/blob/master/README.org#defining-gptel-tools
  (gptel-make-tool
   :name "read_buffer"
   :function (lambda (buffer)
               "Return the contents of BUFFER as a string.
Signals an error if BUFFER does not exist or is not live."
               (if-let ((buf (get-buffer buffer)))
                   (with-current-buffer buf
                     (buffer-substring-no-properties (point-min) (point-max)))
                 (signal 'invalid-argument (list "Buffer does not exist" buffer))))
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  (gptel-make-tool
   :name "lookup_definition"
   :function
   (lambda (symbol)
     "Return the definition of SYMBOL as a string.
SYMBOL can be a function, variable, or face name.
Returns information about the symbol's type and its definition."
     (let* ((sym symbol)
            (result nil))
       (unless sym
         (signal 'invalid-argument (list "Symbol does not exist" symbol)))
       (with-temp-buffer
         ;; Check function definition
         (when (fboundp sym)
           (let ((fn-def (symbol-function sym)))
             (push (format "Function: %s\n%S"
                           (documentation sym)
                           (if (subrp fn-def)
                               "[Built-in function]"
                             fn-def))
                   result)))
         ;; Check variable definition
         (when (boundp sym)
           (push (format "Variable: %s\nValue: %S"
                         (or (documentation-property sym 'variable-documentation)
                             "No documentation")
                         (symbol-value sym))
                 result))
         ;; Check if it's a face
         (when (facep sym)
           (push (format "Face: %s\n%S"
                         (or (documentation-property sym 'face-documentation)
                             "No documentation")
                         (face-all-attributes sym (selected-frame)))
                 result))
         (unless result
           (signal 'invalid-argument
                   (list "Symbol exists but has no definition" symbol)))
         (string-join (nreverse result) "\n\n"))))
   :description "Look up the definition of an Emacs Lisp symbol (function, variable, or face)"
   :args (list '(:name "symbol"
                       :type "string"
                       :description "The name of the Emacs Lisp symbol, face, or function to look up"))
   :category "emacs")

  (gptel-make-tool
   :name "get_buffer_diffs"
   :function (lambda ()
               "Return diffs for modified buffers in the current gptel context."
               (let ((result ""))
                 (dolist (item gptel-context--alist)
                   (let ((buf (if (bufferp item)
                                  item 
                                (if (bufferp (car item))
                                    (car item)
                                  (car (car item))))))
                     (when (buffer-file-name buf)
                       (with-current-buffer buf
                         (when (buffer-modified-p)
                           (setq result
                                 (concat result
                                         (format "\n=== Buffer: %s ===\n" (buffer-name))
                                         (with-temp-buffer
                                           (let ((magit-diff-buffer-file-args '("--no-ext-diff")))
                                             (magit-git-insert "diff-files" "--" (buffer-file-name buf)))
                                           (buffer-string)))))))))
                 (if (string-empty-p result)
                     "No modified buffers found in context"
                   result)))
   :description "Get diffs for modified buffers in the current gptel context"
   :args nil
   :category "emacs")

  (if (work-machine-p)
      (when-let ((key (getenv "GEMINI_API_KEY")))
        (setq gptel-backend
              (gptel-make-gemini "Gemini"
                :stream t 
                :key key
                :models '(gemini-2.0-pro-exp-02-05
                          gemini-2.0-flash-thinking-exp-01-21
                          gemini-2.0-flash))))
    (setq gptel-backend
          (when-let ((key (getenv "ANTHROPIC_API_KEY")))
            (gptel-make-anthropic "Claude"
              :stream t 
              :key key))))

  (setq gptel-track-media t
        gptel-default-mode 'org-mode
        gptel-use-tools t)
  
  (when-let ((file (expand-file-name my/gptel-directives-file user-emacs-directory)))
    (require 'filenotify)
    (file-notify-add-watch
     file
     '(change)
     (lambda (_event)
       (my/load-gptel-directives-from-org))))

  (gptel-make-ollama "Ollama" 
    :host "localhost:11434"
    :stream t
    :models '(deepseek-r1:32b
              deepseek-r1:14b
              llama3.2:latest
              llama3.2-vision:latest))

  (when-let ((key (getenv "CEREBRAS_API_KEY")))
    (gptel-make-openai "Cerebras"
      :host "api.cerebras.ai"
      :endpoint "/v1/chat/completions"
      :stream nil
      :key key
      :models '(llama3.1-70b
                llama3.1-8b
                llama3.3-70b)))
  
  :bind (:map gptel-mode-map
              ("C-c C-g" . gptel-menu)
              ("C-c C-d" . my/load-gptel-directives-from-org)
              ("C-x C-s" . my/gptel-save-buffer)))

(provide 'init-gptel)
