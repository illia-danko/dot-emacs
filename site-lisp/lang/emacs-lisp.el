(require 'api/list)
(require 'edit/core)
(require 'edit/formatting)

(with-eval-after-load 'emacs
  (setq format-all-default-formatters
		(api/upsert-car-string format-all-default-formatters
							   '("Emacs Lisp" emacs-lisp)))
  (add-hook 'emacs-lisp-mode-hook #'format-all-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(provide 'lang/emacs-lisp)
