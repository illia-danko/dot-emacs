(require 'po-mode)

(add-hook 'po-mode-hook #'hl-line-mode)
(add-hook 'po-mode-hook #'display-line-numbers-mode)

(provide 'text/po-mode)
