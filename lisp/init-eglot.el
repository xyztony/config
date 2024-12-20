(use-package prettier-js
  :ensure t
  :hook (((js2-mode typescript-mode tsx-ts-mode typescript-ts-mode js2-jsx-mode) . prettier-js-mode)))

(use-package eglot
  :ensure t
  :hook (((;; clojure-mode clojurec-mode clojurescript-mode
           java-mode
           typescript-ts-mode
           typescript-mode)
          . eglot-ensure)
         ((cider-mode eglot-managed-mode) . eglot-disable-in-cider)
         )
  :preface
  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode) "typescript-language-server" "--stdio"
                 :initializationOptions
                 (:preferences
            (:disableSuggestions                                    :json-false     ;; boolean
             :quotePreference                                       "double"        ;; "auto" | "double" | "single"
             :includeCompletionsForModuleExports                    t               ;; boolean
             :includeCompletionsForImportStatements                 t               ;; boolean
             :includeCompletionsWithSnippetText                     t               ;; boolean
             :includeCompletionsWithInsertText                      t               ;; boolean
             :includeAutomaticOptionalChainCompletions              t               ;; boolean
             :includeCompletionsWithClassMemberSnippets             t               ;; boolean
             :includeCompletionsWithObjectLiteralMethodSnippets     t               ;; boolean
             :useLabelDetailsInCompletionEntries                    t               ;; boolean
             :allowIncompleteCompletions                            t               ;; boolean
             :importModuleSpecifierPreference                       "shortest"      ;; "shortest" | "project-relative" | "relative" | "non-relative"
             :importModuleSpecifierEnding                           "minimal"       ;; "auto" | "minimal" | "index" | "js"
             :allowTextChangesInNewFiles                            t               ;; boolean
             :providePrefixAndSuffixTextForRename                   t               ;; boolean
             :provideRefactorNotApplicableReason                    :json-false     ;; boolean
             :allowRenameOfImportPath                               t               ;; boolean
             :jsxAttributeCompletionStyle                           "auto"          ;; "auto" | "braces" | "none"
             :displayPartsForJSDoc                                  t               ;; boolean
             :generateReturnInDocTemplate                           t               ;; boolean
             :includeInlayParameterNameHints                        "all"           ;; "none" | "literals" | "all"
             :includeInlayParameterNameHintsWhenArgumentMatchesName t               ;; boolean
             :includeInlayFunctionParameterTypeHints                t               ;; boolean,
             :includeInlayVariableTypeHints                         t               ;; boolean
             :includeInlayVariableTypeHintsWhenTypeMatchesName      t               ;; boolean
             :includeInlayPropertyDeclarationTypeHints              t               ;; boolean
             :includeInlayFunctionLikeReturnTypeHints               t               ;; boolean
             :includeInlayEnumMemberValueHints                      t               ;; boolean
             :disableLineTextInReferences                           :json-false))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet)))

(provide 'init-eglot)
