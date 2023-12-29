(require 'eglot)
(require 'flymake)
(require 'api/macro)

(api/customize-set-variable* 'eglot-stay-out-of '(flymake))

(provide 'completion/lsp)
