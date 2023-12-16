(require 'api/list)
(require 'edit/treesit)
(require 'edit/formatting)

(progn
  (with-eval-after-load 'yaml-ts-mode
	(add-to-list 'treesit-language-source-alist '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))

	(setq format-all-default-formatters
		  (api/upsert-car-string format-all-default-formatters
								 `("YAML" (prettier ,edit/formatting-prettier-config-path))))
	(add-hook 'yaml-ts-mode-hook #'format-all-mode))

  (require 'yaml-ts-mode))

(provide 'text/yaml)
