(with-eval-after-load 'completion/minibuffer
  (define-key vertico-map (kbd "M-r") #'vertico-exit-input))

(with-eval-after-load 'tool/filesystem
  (define-key dired-mode-map "O" #'tool/dired-system-open))

(provide 'keymap/common)
