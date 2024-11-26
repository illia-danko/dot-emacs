(require 'auto-dark)
(require 'ui/face)
(require 'api/macro)

(api/customize-set-variable*
 'auto-dark-dark-theme ui/theme-dark-variant
 'auto-dark-light-theme ui/theme-light-variant)

(add-hook 'auto-dark-dark-mode-hook
		  (lambda ()
			(setq catppuccin-flavor 'mocha)
			(catppuccin-reload)))
(add-hook 'auto-dark-light-mode-hook
		  (lambda ()
			(setq catppuccin-flavor 'latte)
			(catppuccin-reload)))

(auto-dark-mode t)

(provide 'extras/system-theme)
