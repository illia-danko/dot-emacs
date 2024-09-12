(require 'core/core)
(require 'hl-line)

(add-hook 'conf-mode-hook #'hl-line-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(provide 'text/conf)
