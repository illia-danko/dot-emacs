(use-package elixir-mode :straight t
  :hook
  (elixir-mode . inf-elixir-minor-mode))

(use-package inf-elixir :straight t)

(provide 'init-elixir)
