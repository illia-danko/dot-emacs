;; Format on save.
(use-package format-all :straight t
  :hook
  ((go-mode js-mode typescript-mode react-mode markdown-mode yaml-mode emacs-lisp-mode) . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)

  :custom
  (format-all-show-errors 'never) ; do not show message on error
  ;; Formatters settings.
  (format-all-default-formatters `(("Go" goimports)
                                   ("YAML" (prettier ,my-prettier-config-path))
                                   ("Markdown" (prettier ,my-prettier-config-path))
                                   ("TSX" (prettier))
                                   ("Emacs Lisp" emacs-lisp)
                                   ("TypeScript" prettier)
                                   ("JavaScript" prettier)
                                   ("JSX" prettier)
                                   )))

(provide 'init-formatting)
