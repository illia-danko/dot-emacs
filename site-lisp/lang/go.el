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
(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'go-ts-mode-hook #'format-all-mode)

;; Define `golines' formatter.
(define-format-all-formatter golines
  (:executable "golines")
  (:install "go install github.com/segmentio/golines@latest")
  (:languages "Go")
  (:features)
  (:format (format-all--buffer-easy executable "-m" "150"))) ; --max-len=150

;; NOTE: we could use both formatters `goimports' and `golines' as '("Go" goimports
;; golines). However, `goimports' is integrated with `golines', so it is redundant.
(setq format-all-default-formatters
	  (api/upsert-car-string format-all-default-formatters
							 '("Go" golines)))


(provide 'lang/go)
