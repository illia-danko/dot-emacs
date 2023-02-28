;; Strart Emacs maximized, use custom font.
(use-package frame
  :init
  (defun my-adjust-created-frame ()
	(let ((font-size (pcase system-type
					   ('darwin 160)
					   (_ 125)
					   )))
	  (custom-set-faces
	   `(default ((t (:family "Iosevka Nerd Font Mono" :height ,font-size :weight semibold))))
	   `(fixed-pitch ((t (:family "Iosevka Nerd Font Mono" :height ,font-size :weight semibold))))
	   `(variable-pitch ((t (:family "Iosevka Nerd Font Mono" :height ,font-size :weight semibold))))))
    (toggle-frame-maximized))

  :hook
  (window-setup . my-adjust-created-frame))

;; Load theme based on the system theme's variant.
(use-package auto-dark :straight t
  :custom
  (auto-dark-light-theme 'modus-operandi)
  (auto-dark-dark-theme 'modus-vivendi)
  :config
  (auto-dark-mode t))

(provide 'init-appearance)
