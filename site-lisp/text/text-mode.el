(require 'core/core)
(require 'hl-line)

(progn
  (with-eval-after-load 'text-mode
	(add-hook 'text-mode-hook #'hl-line-mode)))

(provide 'text/text-mode)
