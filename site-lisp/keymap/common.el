(require 'core/intercept-mode)

(with-eval-after-load 'emacs
  (global-set-key [remap kill-buffer] #'kill-this-buffer))

(with-eval-after-load 'completion/minibuffer
  (define-key vertico-map (kbd "M-r") #'vertico-exit-input))

(with-eval-after-load 'tool/core
  (global-set-key (kbd "C-c b") #'recompile)
  (global-set-key (kbd "C-c B") #'project-compile))

(with-eval-after-load 'tool/filesystem
  (global-set-key [remap dired] #'dired-jump)
  (define-key dired-mode-map "O" #'tool/dired-system-open))

(with-eval-after-load 'tool/vterm
  (global-set-key (kbd "C-c t") #'tool/vterm-project)
  (global-set-key (kbd "C-c T") #'vterm))

(with-eval-after-load 'edit/core
  (define-key core/intercept-mode-map (kbd "C-w") #'edit/backward-kill-word-or-region)
  (global-set-key (kbd "C-c SPC") #'ace-jump-mode)
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp))

(provide 'keymap/common)
