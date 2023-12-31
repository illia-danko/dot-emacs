(require 'completion/lsp)
(require 'edit/formatting)

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'format-all-mode)
(add-hook 'c++-mode-hook #'format-all-mode)


(provide 'lang/c)
