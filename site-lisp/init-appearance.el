;; Strart Emacs maximized, use custom font.

(when window-system
  (setq my-default-font "-*-JetBrainsMono Nerd Font-semibold-normal-normal-*-15-*-*-*-p-0-iso10646-1")
  (set-face-attribute 'default nil :font my-default-font)
  (toggle-frame-maximized))

(use-package ef-themes :straight t)

(unless window-system
  (defun my-apply-theme (&optional frame)
	"Adjust faces."
	(when frame
      (select-frame frame))
	(mapc #'disable-theme custom-enabled-themes)
	(load-theme 'ef-deuteranopia-light t))

  (my-apply-theme)
  (add-hook 'after-make-frame-functions #'my-apply-theme))

(defun my-apply-theme-ns (appearance)
  "Load theme based on the system theme's variant."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ef-deuteranopia-light t))
    ('dark (load-theme 'ef-night t))))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme-ns)

(provide 'init-appearance)
