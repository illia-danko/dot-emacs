;; Format on save.
(use-package format-all :straight t
  :hook
  ((go-mode) . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)
  :custom
  (format-all-show-errors 'never) ; do not show message on error
  (format-all-default-formatters `(("Go" goimports))) ; formatters settings.
  )

(provide 'init-formatting)
