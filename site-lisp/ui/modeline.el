(require 'doom-modeline)
(require 'api/macro)

(api/customize-set-variable* 'doom-modeline-height 28
							 'doom-modeline-bar-width 4
							 'doom-modeline-icon nil)
(doom-modeline-mode 1)

(provide 'ui/modeline)
