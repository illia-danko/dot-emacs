(use-package markdown-mode :straight t
  :init
  (defun my-markdown-toggle-fontifications (&optional arg)
    "Toggle fontifications on/off."
    (interactive (list (or current-prefix-arg 'toggle)))
    (markdown-toggle-markup-hiding arg))

  :bind
  (:map markdown-mode-map
        ("C-c 4" . my-markdown-toggle-fontifications))

  :custom
  (markdown-fontify-code-blocks-natively t) ; highlight code block syntax
  (markdown-hide-markup t)                  ; hide urls
  )

(provide 'init-markdown)
