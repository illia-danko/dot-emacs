(require 'elisp-mode)

(require 'api/list)
(require 'edit/core)
(require 'edit/formatting)

(add-hook 'emacs-lisp-mode-hook #'format-all-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(provide 'lang/emacs-lisp)
