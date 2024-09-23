(require 'completion/lsp)
(require 'terraform-mode)

(api/customize-set-variable*
 'terraform-indent-level 2)

(defun lang/terrform-mode-hook ()
  (eglot-ensure)
  (format-all-mode 1))

(add-hook 'terraform-mode-hook #'lang/terrform-mode-hook)

(provide 'lang/terraform)
