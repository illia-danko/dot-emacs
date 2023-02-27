;; LSP client.
(use-package eglot :straight t
  :hook
  ((go-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '(flymake)))

(provide 'init-lsp)
