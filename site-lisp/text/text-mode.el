(require 'text-mode)
(require 'core/core)
(require 'hl-line)

(add-hook 'text-mode-hook #'hl-line-mode)

(provide 'text/text-mode)
