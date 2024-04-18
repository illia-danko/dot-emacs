;; Each `emacsclient' instance uses individual buffers/settings scope.

(require 'perspective)
(require 'api/macro)

(api/customize-set-variable* 'persp-mode-prefix-key (kbd "C-x ~")) ; often not used, but it is required by the package settings.
(persp-mode 1)

;; Hook with `consult'.
(with-eval-after-load 'consult
  (consult-customize consult--source-buffer :hidden t :default nil)
  (setq consult-buffer-sources `(persp-consult-source)))

(provide 'core/scope)
