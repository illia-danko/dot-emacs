(with-eval-after-load 'completion/minibuffer
  (define-key vertico-map (kbd "M-r") #'vertico-exit-input))

(with-eval-after-load 'tool/filesystem
  (define-key dired-mode-map "O" #'tool/dired-system-open))

(with-eval-after-load 'edit/core
  (global-set-key (kbd "M-t") #'ace-jump-mode)
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp))

(provide 'keymap/common)
