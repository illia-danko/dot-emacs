(require 'eglot)
(require 'flymake)
(require 'api/macro)

(api/customize-set-variable* 'eglot-stay-out-of '(flymake eldoc))

;; Override `eglot-inlay-hints-mode' to avoid warning of the not used feature.
(defun eglot-inlay-hints-mode (&optional mode))

(provide 'completion/lsp)
