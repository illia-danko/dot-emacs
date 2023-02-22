;; LSP client.
(use-package eglot :straight t
  :hook
  ((go-mode) . eglot-ensure))

(provide 'init-lsp)
