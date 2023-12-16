(require 'core/core)
(require 'hl-line)

(progn
  (with-eval-after-load 'prog-mode
	(add-hook 'prog-mode-hook #'hl-line-mode)
	(add-hook 'prog-mode-hook #'display-line-numbers-mode)))

(provide 'lang/prog-mode)
