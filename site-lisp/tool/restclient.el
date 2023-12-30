(require 'restclient)

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(add-hook 'restclient-mode-hook #'display-line-numbers-mode)

(provide 'tool/restclient)
