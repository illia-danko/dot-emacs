(require 'nix-mode)
(require 'completion/lsp)
(require 'edit/formatting)
(require 'api/macro)

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook #'format-all-mode)

(setq format-all-default-formatters
	  (api/upsert-car-string format-all-default-formatters
							 '("Nix" nixfmt)))

(provide 'lang/nix)
