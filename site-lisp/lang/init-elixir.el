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

  (defun my-inf-elixir-switch-repl ()
    (interactive)
    (let* ((inf-elixir-repl-buffer-identifier "*Inf-Elixir")
           (current-project-buffers (project-buffers (project-current)))
           (inf-elixir-repl-buffers (seq-filter (lambda (buf)
                                                  (string-prefix-p inf-elixir-repl-buffer-identifier
                                                                   (buffer-name buf)))
                                                current-project-buffers))
           (inf-elixir-repl-buffer (car inf-elixir-repl-buffers)))
      (if inf-elixir-repl-buffer
          (switch-to-buffer-other-window inf-elixir-repl-buffer)
        (inf-elixir-set-repl))))

  (defun my-inf-elixir-switch-to-source-buffer ()
    (interactive)
    (switch-to-buffer-other-window (other-buffer (current-buffer) 1)))

  :custom
  (inf-elixir-on-send-switch-to-repl nil) ; do not auto switch to the repl buffer

  :bind
  ((:map elixir-mode-map
         ("C-c C-b" . inf-elixir-send-buffer)
         ("C-c C-c" . my-inf-elixir-eval)
         ("C-c C-z" . my-inf-elixir-switch-repl))
   (:map inf-elixir-mode-map
         ("C-c C-z" . my-inf-elixir-switch-to-source-buffer))))

(provide 'init-elixir)
