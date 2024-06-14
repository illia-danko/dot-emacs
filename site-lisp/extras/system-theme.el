(require 'auto-dark)
(require 'ui/face)
(require 'api/macro)

(api/customize-set-variable*
 'auto-dark-dark-theme ui/theme-dark-variant
 'auto-dark-light-theme ui/theme-light-variant)

(auto-dark-mode 1)

(provide 'extras/system-theme)
