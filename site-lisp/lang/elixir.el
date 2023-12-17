(require 'completion/lsp)
(require 'edit/formatting)
(require 'edit/treesit)
(require 'api/variable)

(progn
  (with-eval-after-load 'elixir-ts-mode
	(advice-add 'edit/treesit-install-language-grammar :after (lambda (&rest args)
																(elixir-ts-install-grammar)))

	;; Use `elixir-ls' over the default `language_server.sh'.
	(cl-pushnew '((elixir-ts-mode heex-ts-mode) . ("elixir-ls"))
				eglot-server-programs
				:test #'equal)

	(add-hook 'elixir-ts-mode-hook #'eglot-ensure)
	(add-hook 'heex-ts-mode-hook #'eglot-ensure)

	(defun lang/elixir-heex-format-all-hook ()
	  ;; For some reason `format-all' treats HEEx buffer as HTML...
	  (setq-local format-all-default-formatters
				  (api/upsert-car-string format-all-default-formatters
										 '("HTML" mix-format)))
	  (format-all-mode))

	(add-hook 'elixir-ts-mode-hook #'format-all-mode)
	(add-hook 'heex-ts-mode-hook #'lang/elixir-heex-format-all-hook))

  (require 'elixir-ts-mode))

(progn
  (with-eval-after-load 'inf-elixir
	(defun lang/elixir-eval-line-or-region (&optional arg)
      (interactive "p")
      (if mark-active
		  (progn (inf-elixir-send-region) (deactivate-mark))
		(inf-elixir-send-line)))

	(api/customize-set-variable* 'inf-elixir-switch-to-repl-on-send nil))

  (require 'inf-elixir))

(provide 'lang/elixir)
