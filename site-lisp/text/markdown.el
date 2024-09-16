(require 'markdown-mode)
(require 'api/macro)

(defun text/markdown-toggle-fontifications (&optional arg)
  "Toggle fontifications on/off."
  (interactive (list (or current-prefix-arg 'toggle)))
  (markdown-toggle-markup-hiding arg))

(api/customize-set-variable* 'markdown-fontify-code-blocks-natively t ; highlight code block syntax
							 'markdown-hide-markup t)                  ; hide urls

(defun text/markdown-mode-hook ()
  (outline-hide-body)
  (setq-local fill-column 80))

(add-hook 'markdown-mode-hook #'text/markdown-mode-hook)


(provide 'text/markdown)
