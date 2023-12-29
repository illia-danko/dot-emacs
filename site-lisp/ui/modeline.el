(require 'doom-modeline)
(require 'api/variable)

(api/customize-set-variable* 'doom-modeline-height 28)
(doom-modeline-mode 1)

(provide 'ui/modeline)
