(require 'lua-mode)
(require 'completion/lsp)
(require 'edit/formatting)

(defun lang/lua-setup-hook ()
  (setq-local tab-width 2)
  (eglot-ensure)
  (format-all-mode))

(add-hook 'lua-mode-hook #'lang/lua-setup-hook)

(provide 'lang/lua)
