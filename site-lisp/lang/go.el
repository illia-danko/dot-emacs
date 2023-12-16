(require 'edit/treesit)
(require 'completion/lsp)

(progn
  (with-eval-after-load 'go-ts-mode
	(customize-set-variable 'go-ts-mode-indent-offset 4)
	(add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
	(add-hook 'go-ts-mode-hook #'eglot-ensure)

	;; TODO(idanko): add `golines' formatter.
	(setq format-all-default-formatters
		  (api/upsert-car-string format-all-default-formatters
								 '("Go" goimports)))
	(add-hook 'go-ts-mode-hook #'format-all-mode))

  (require 'go-ts-mode))

(provide 'lang/go)
