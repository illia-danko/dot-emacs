(use-package wgrep :straight t
  :custom
  (wgrep-auto-save-buffer t) ; automatically save buffers after edit
  )

(use-package rg :straight t
  :after (wgrep))

(provide 'init-rg)
