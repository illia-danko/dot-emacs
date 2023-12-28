(require 'markdown-mode)
(require 'api/variable)

(defun text/markdown-toggle-fontifications (&optional arg)
  "Toggle fontifications on/off."
  (interactive (list (or current-prefix-arg 'toggle)))
  (markdown-toggle-markup-hiding arg))

(api/customize-set-variable* 'markdown-fontify-code-blocks-natively t ; highlight code block syntax
							 'markdown-hide-markup t)                  ; hide urls

(add-hook 'markdown-mode-hook #'outline-hide-other)


(provide 'text/markdown)
