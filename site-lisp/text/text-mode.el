(require 'text-mode)
(require 'core/core)
(require 'hl-line)

(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(provide 'text/text-mode)
