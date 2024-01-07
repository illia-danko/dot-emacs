(require 'prog-mode)
(require 'core/core)
(require 'hl-line)

(add-hook 'prog-mode-hook #'hl-line-mode)


(provide 'lang/prog-mode)
