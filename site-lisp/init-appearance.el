;; Strart Emacs maximized, use custom font.

(when window-system
  (setq my-default-font "-*-JetBrainsMono Nerd Font-semibold-normal-normal-*-15-*-*-*-p-0-iso10646-1")
  (set-face-attribute 'default nil :font my-default-font)
  (toggle-frame-maximized))

(use-package spacemacs-theme :straight t
  :defer t
  :config
  (mapc #'disable-theme custom-enabled-themes))

(load-theme 'spacemacs-dark t)

;; Load theme based on the system theme's variant.
(use-package auto-dark :straight t
  :after (spacemacs-theme)
  :if (display-graphic-p)
  :custom
  (auto-dark-light-theme 'spacemacs-light)
  (auto-dark-dark-theme 'spacemacs-dark)
  :config
  (auto-dark-mode t))

(provide 'init-appearance)
