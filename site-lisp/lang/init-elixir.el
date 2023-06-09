(use-package elixir-mode :straight t
  :hook
  (elixir-mode . inf-elixir-minor-mode))

(use-package inf-elixir :straight t
  :init
  (defun my-inf-elixir-eval (&optional arg)
    (interactive "p")
    (if mark-active
		(inf-elixir-send-region)
      (inf-elixir-send-line)))

  :custom
  (inf-elixir-on-send-switch-to-repl nil) ; do not auto switch to the repl buffer

  :bind
  (:map elixir-mode-map
        ("C-c C-b" . inf-elixir-send-buffer)
        ("C-c C-c" . my-inf-elixir-eval)))

(provide 'init-elixir)
