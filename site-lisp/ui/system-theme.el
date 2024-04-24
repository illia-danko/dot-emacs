(require 'auto-dark)
(require 'ui/face)
(require 'api/macro)

(api/customize-set-variable*
 'auto-dark-dark-theme ui/theme-dark-variant
 'auto-dark-light-theme ui/theme-light-variant)

(if (display-graphic-p)
	(auto-dark-mode 1))


(provide 'ui/system-theme)
