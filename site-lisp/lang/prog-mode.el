(require 'hl-line)

(progn
  (with-eval-after-load 'prog-mode
	(add-hook 'prog-mode-hook #'hl-line-mode)))

(provide 'lang/prog-mode)
