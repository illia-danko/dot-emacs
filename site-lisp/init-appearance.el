;; Strart Emacs maximized, use custom font.

(when window-system
  (setq my-default-font "-JB-JetBrainsMono Nerd Font Mono-semibold-normal-normal-*-32-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font my-default-font)
  (toggle-frame-maximized))

(use-package doom-themes :straight t)

(when (or (eq system-type 'gnu/linux) )
  (defun my-apply-theme (&optional frame)
	"Adjust faces."
	(when frame
      (select-frame frame))
	(mapc #'disable-theme custom-enabled-themes)
	(load-theme 'doom-one-light t))

  (my-apply-theme)
  (add-hook 'after-make-frame-functions #'my-apply-theme))

;; Better modeline look and feel.
(use-package doom-modeline :straight t
  :config (doom-modeline-mode 1))

;; Macos auto theme hook.
(defun my-apply-theme-ns (appearance)
  "Load theme based on the system theme's variant."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-one-light t))
    ('dark (load-theme 'doom-one t))))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme-ns)

(provide 'init-appearance)
