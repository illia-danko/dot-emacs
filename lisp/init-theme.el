;; Strart Emacs maximized, use custom font.
(use-package frame
  :init
  (defun my-adjust-created-frame ()
	(let ((font-code (if (eq system-type 'darwin)
						 "-*-Iosevka Nerd Font Mono-semibold-normal-normal-*-16-*-*-*-m-0-iso10646-1"
					   "-*-Iosevka Nerd Font Mono-semibold-normal-normal-*-32-*-*-*-m-0-iso10646-1")))
	  (set-frame-font font-code nil t))
    (toggle-frame-maximized))

  :hook
  (window-setup . my-adjust-created-frame))

;; Load theme based on the system theme's variant on MACOS.
(use-package frame
  :if (eq system-type 'darwin)
  :init
  (defun my-apply-theme (appearance)
	"Load theme, taking current system APPEARANCE into consideration.
See https://github.com/d12frosted/homebrew-emacs-plus for details."
	(mapc #'disable-theme custom-enabled-themes)
	(pcase appearance
      ('light (load-theme 'modus-operandi t))
      ('dark (load-theme 'modus-vivendi t))))

  (add-hook 'ns-system-appearance-change-functions #'my-apply-theme))

(provide 'init-theme)
