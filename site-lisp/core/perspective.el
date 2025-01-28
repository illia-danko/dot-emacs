;; Each `emacsclient' instance uses individual buffers/settings scope.

(require 'perspective)
(require 'api/macro)

(api/customize-set-variable* 'persp-mode-prefix-key (kbd "C-x ~")) ; often not used, but it is required by the package settings.
(persp-mode 1)

(provide 'core/perspective)
