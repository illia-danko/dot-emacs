(require 'go-ts-mode)
(require 'api/list)
(require 'api/macro)
(require 'edit/treesit)
(require 'completion/lsp)
(require 'edit/formatting)


;; Generic settings.
(api/customize-set-variable*
 'go-ts-mode-indent-offset 4)

(add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
(add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
(add-hook 'go-ts-mode-hook (lambda ()
							 (eglot-ensure)
							 (add-hook 'before-save-hook #'eglot-format)))


(provide 'lang/go)
