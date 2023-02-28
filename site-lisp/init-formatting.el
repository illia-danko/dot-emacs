;; Format on save.
(use-package format-all :straight t
  :hook
  ((go-mode yaml-mode) . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)
  :custom
  (format-all-show-errors 'never) ; do not show message on error
  ;; formatters settings.
  (format-all-default-formatters `(("Go" goimports) ("YAML" (prettier ,my-prettier-config-path))))
  )

(provide 'init-formatting)
