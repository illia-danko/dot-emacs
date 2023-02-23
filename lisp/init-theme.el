;; Strart Emacs maximized, use custom font.
(use-package frame
  :init
  (defun my-adjust-created-frame ()
    (set-frame-font
     "-*-Iosevka Nerd Font Mono-semibold-normal-normal-*-16-*-*-*-m-0-iso10646-1" nil t)
    (toggle-frame-maximized))
  :hook
  (window-setup . my-adjust-created-frame)
  :config
  (load-theme 'modus-operandi t nil))

(provide 'init-theme)
