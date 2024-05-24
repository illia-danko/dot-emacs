(require 'inf-elixir)

(require 'completion/lsp)
(require 'edit/treesit)
(require 'api/macro)
(require 'elixir-ts-mode) ;; required for importing elixir-ts-mode-map symbol

(add-to-list 'treesit-language-source-alist '(elixir "https://github.com/elixir-lang/tree-sitter-elixir"))
(add-to-list 'treesit-language-source-alist '(heex "https://github.com/phoenixframework/tree-sitter-heex"))

;; elixir-ts-mode.
(advice-add 'edit/treesit-install-language-grammar :after (lambda (&rest args)
															(elixir-ts-install-grammar)))

;; Use `elixir-ls' over the default `language_server.sh'.
(cl-pushnew '((elixir-ts-mode heex-ts-mode) . ("elixir-ls"))
			eglot-server-programs
			:test #'equal)

(defun tool/elixir-mode-setup ()
  (eglot-ensure)
  (format-all-mode))

(add-hook 'elixir-ts-mode-hook #'tool/elixir-mode-setup)
(add-hook 'heex-ts-mode-hook #'tool/elixir-mode-setup)

;; inf-elixir.

(defun lang/elixir-eval-line-or-region (&optional arg)
  (interactive "p")
  (if mark-active
	  (progn (inf-elixir-send-region) (deactivate-mark))
	(inf-elixir-send-line)))

(api/customize-set-variable* 'inf-elixir-switch-to-repl-on-send nil)

(defun lang/inf-elixir-phoenix-server ()
  (interactive)
  (let ((inf-elixir-project-command "iex -S mix phx.server"))
	(inf-elixir-project)))

(provide 'lang/elixir)
