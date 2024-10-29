(use-package gptel
  :custom
  (gptel-model 'claude-3-5-sonnet-20241022)
  
  :config
  (setq gptel-track-media t)
  
  (gptel-make-ollama "Ollama" 
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5:latest
             llama3.2:3b-text-q8_0 
             llama3.1:latest
             qwen2.5-coder:7b-instruct-q8_0
             llava-llama3:latest))

  (when-let ((key (getenv "ANTHROPIC_API_KEY")))
    (gptel-make-anthropic "Claude"
      :stream t 
      :key key))

  (defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

  :bind (:map gptel-mode-map
         ("C-c C-g" . gptel-menu)))

(provide 'init-gptel)
