(with-eval-after-load 'completion/minibuffer
  (define-key vertico-map (kbd "M-r") #'vertico-exit-input))

(with-eval-after-load 'tools/filesystem
  (define-key dired-mode-map "O" #'tools/dired-system-open))

(provide 'keymap/common)
