(defcustom my/gptel-directives-file "gptel/gptel-directives.org"
  "Path to the gptel directives org file, relative to =user-emacs-directory'."
  :type 'string
  :group 'gptel)

(defun my/gptel-save-buffer ()
  "Save the current GPTEL buffer with the default directory set to ~/Documents/notes."
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

(use-package gptel
  :custom
  (gptel-model 'gemini-2.0-flash-exp)
    
  :config
  (my/load-gptel-directives-from-org)
  (when-let ((key (getenv "GEMINI_API_KEY")))
    (setq gptel-backend
          (gptel-make-gemini "Gemini"
            :stream t 
            :key key
            :models '(gemini-2.0-flash-thinking-exp-1219
                      gemini-exp-1206
                      gemini-2.0-flash-exp
                      gemini-1.5-flash
                      gemini-1.5-pro))))
  (setq gptel-track-media t
        gptel-default-mode 'org-mode)
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
    :models '(deepseek-r1:14b
              llama3.1:latest
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

  (when-let ((key (getenv "ANTHROPIC_API_KEY")))
    (gptel-make-anthropic "Claude"
      :stream t 
      :key key))

  

  (when-let ((key (getenv "XAI_API_KEY")))
      (gptel-make-openai "xAI"
        :host "api.x.ai"
        :key key
        :endpoint "/v1/chat/completions"
        :stream t
        :models '(grok-beta
                  grok-2-1212
                  grok-2-vision-1212)))

  :bind (:map gptel-mode-map
              ("C-c C-g" . gptel-menu)
              ("C-c C-d" . my/load-gptel-directives-from-org)
              ("C-x C-s" . my/gptel-save-buffer)))

(provide 'init-gptel)
