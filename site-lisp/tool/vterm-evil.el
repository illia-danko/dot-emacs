(require 'tool/vterm)
(require 'evil)
(require 'api/macro)

(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'vterm-copy-mode 'normal)

(api/customize-set-variable*
 'vterm-max-scrollback 30000)

(defun tool/vterm-evil-exit-copy-mode (&optional arg)
  (interactive)
  (vterm-copy-mode-done arg)
  (evil-emacs-state))

(defun tool/vterm-evil-copy-mode-normal-mode (&optional arg)
  (interactive)
  (call-interactively 'vterm-copy-mode arg)
  (evil-normal-state))

(provide 'tool/vterm-evil)
