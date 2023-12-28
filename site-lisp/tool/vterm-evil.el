(require 'tool/vterm)
(require 'evil)

(evil-set-initial-state 'vterm-mode 'emacs)

(defun tool/vterm-evil-exit-copy-mode (&optional arg)
  (interactive)
  (vterm-copy-mode-done arg)
  (evil-emacs-state))

(provide 'tool/vterm-evil)
