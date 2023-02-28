;; Strart Emacs maximized, use custom font.

(when window-system
  (setq my-default-font "-apple-Monaco-medium-normal-normal-*-15-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font my-default-font))

(toggle-frame-maximized)


;; Load theme based on the system theme's variant.
(use-package auto-dark :straight t
  :custom
  (auto-dark-light-theme 'modus-operandi)
  (auto-dark-dark-theme 'modus-vivendi)
  :config
  (auto-dark-mode t))

(provide 'init-appearance)
