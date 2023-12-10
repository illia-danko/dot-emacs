(progn
  (with-eval-after-load 'eglot
    (customize-set-variable 'eglot-stay-out-of '(flymake)))

  (require 'eglot))

(provide 'completion/lsp)
