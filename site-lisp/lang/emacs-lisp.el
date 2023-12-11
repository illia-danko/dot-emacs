(require 'api/list)

(with-eval-after-load 'edit/formatting
  (setq format-all-default-formatters
	(api/upsert-car-string format-all-default-formatters
				 '("Emacs Lisp" emacs-lisp)))

  (add-hook 'emacs-lisp-mode-hook #'format-all-mode))

(provide 'lang/emacs-lisp)
