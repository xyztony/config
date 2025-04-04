(use-package prettier-js
  :ensure t
  :hook (((js2-mode typescript-mode tsx-ts-mode typescript-ts-mode js2-jsx-mode) . prettier-js-mode)))

(use-package eglot
  :ensure t
  :preface
  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  
  :hook (((java-mode
           fsharp-mode
           typescript-ts-mode
           typescript-mode)
          . eglot-ensure)
         ((cider-mode eglot-managed-mode) . eglot-disable-in-cider))
  
  :config
  (add-to-list 'eglot-server-programs
               '((html-mode html-ts-mode) "vscode-html-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode) "typescript-language-server" "--stdio"
                 :initializationOptions
                 (:preferences
            (:disableSuggestions                                    :json-false
             :quotePreference                                       "double"
             :includeCompletionsForModuleExports                    t
             :includeCompletionsForImportStatements                 t
             :includeCompletionsWithSnippetText                     t
             :includeCompletionsWithInsertText                      t
             :includeAutomaticOptionalChainCompletions              t
             :includeCompletionsWithClassMemberSnippets             t
             :includeCompletionsWithObjectLiteralMethodSnippets     t
             :useLabelDetailsInCompletionEntries                    t
             :allowIncompleteCompletions                            t
             :importModuleSpecifierPreference                       "shortest"
             :importModuleSpecifierEnding                           "minimal"
             :allowTextChangesInNewFiles                            t
             :providePrefixAndSuffixTextForRename                   t
             :provideRefactorNotApplicableReason                    :json-false
             :allowRenameOfImportPath                               t
             :jsxAttributeCompletionStyle                           "auto"
             :displayPartsForJSDoc                                  t
             :generateReturnInDocTemplate                           t
             :includeInlayParameterNameHints                        "all"
             :includeInlayParameterNameHintsWhenArgumentMatchesName t
             :includeInlayFunctionParameterTypeHints                t
             :includeInlayVariableTypeHints                         t
             :includeInlayVariableTypeHintsWhenTypeMatchesName      t
             :includeInlayPropertyDeclarationTypeHints              t
             :includeInlayFunctionLikeReturnTypeHints               t
             :includeInlayEnumMemberValueHints                      t
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
     :foldingRangeProvider)))

(provide 'init-eglot)
