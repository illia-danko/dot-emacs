(require 'eglot)
(require 'flymake)
(require 'api/variable)

(api/customize-set-variable* 'eglot-stay-out-of '(flymake))

(provide 'completion/lsp)
