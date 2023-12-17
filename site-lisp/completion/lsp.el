(require 'api/variable)

(progn
  (with-eval-after-load 'eglot
    (api/customize-set-variable* 'eglot-stay-out-of '(flymake)))

  (require 'eglot))

(provide 'completion/lsp)
