(progn
  (with-eval-after-load 'markdown-mode
	(defun text/markdown-toggle-fontifications (&optional arg)
      "Toggle fontifications on/off."
      (interactive (list (or current-prefix-arg 'toggle)))
      (markdown-toggle-markup-hiding arg))

	(customize-set-variable 'markdown-fontify-code-blocks-natively t) ; highlight code block syntax
	(customize-set-variable 'markdown-hide-markup t)                  ; hide urls
	(add-hook 'markdown-mode-hook #'outline-hide-other))

  (require 'markdown-mode))

(provide 'text/markdown)
