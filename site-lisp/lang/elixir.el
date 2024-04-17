(require 'inf-elixir)

(require 'completion/lsp)
(require 'edit/formatting)
(require 'edit/treesit)
(require 'api/macro)

(add-to-list 'treesit-language-source-alist '(elixir "https://github.com/elixir-lang/tree-sitter-elixir"))
(add-to-list 'treesit-language-source-alist '(heex "https://github.com/phoenixframework/tree-sitter-heex"))
(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
(add-to-list 'major-mode-remap-alist '(heex-mode . heex-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex?\\'" . heex-mode))
(add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-mode))

;; Use `elixir-ls' over the default `language_server.sh'.
(cl-pushnew '((elixir-ts-mode heex-ts-mode) . ("elixir-ls"))
			eglot-server-programs
			:test #'equal)

(add-hook 'elixir-ts-mode-hook (lambda ()
								 (eglot-ensure)
								 (add-hook 'before-save-hook #'eglot-format)))

(add-hook 'heex-ts-mode-hook (lambda ()
							   (eglot-ensure)
							   (add-hook 'before-save-hook #'eglot-format)))


;; inf-elixir.
(defun lang/elixir-eval-line-or-region (&optional arg)
  (interactive "p")
  (if mark-active
	  (progn (inf-elixir-send-region) (deactivate-mark))
	(inf-elixir-send-line)))

(api/customize-set-variable* 'inf-elixir-switch-to-repl-on-send nil)


(provide 'lang/elixir)
