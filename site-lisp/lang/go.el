(require 'api/list)
(require 'edit/treesit)
(require 'completion/lsp)
(require 'edit/formatting)

(progn
  (with-eval-after-load 'go-ts-mode
	(customize-set-variable 'go-ts-mode-indent-offset 4)
	(add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
	(add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
	(add-hook 'go-ts-mode-hook #'eglot-ensure)

	;; TODO(idanko): add `golines' formatter.
	(setq format-all-default-formatters
		  (api/upsert-car-string format-all-default-formatters
								 '("Go" goimports)))
	(add-hook 'go-ts-mode-hook #'format-all-mode))

  (require 'go-ts-mode))

(provide 'lang/go)
