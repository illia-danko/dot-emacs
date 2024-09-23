(require 'go-mode)
(require 'api/macro)
(require 'completion/lsp)
(require 'edit/formatting)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; Define `golines' formatter.
(define-format-all-formatter golines
  (:executable "golines")
  (:install "go install github.com/segmentio/golines@latest")
  (:languages "Go")
  (:features)
  (:format (format-all--buffer-easy executable "-m" "150"))) ; --max-len=150

;; NOTE: we could use both formatters `goimports' and `golines' as '("Go" goimports
;; golines). However, `golines' is superset of `goimports', so this is redundant.
(setq format-all-default-formatters
	  (api/upsert-car-string format-all-default-formatters
							 '("Go" golines)))

(defun lang/go-mode-hook ()
  (setq-local fill-column 120)
  (eglot-ensure)
  (format-all-mode 1))

(add-hook 'go-mode-hook #'lang/go-mode-hook)

(provide 'lang/go)
