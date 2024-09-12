(require 'core/core)
(require 'protobuf-mode)

(add-hook 'protobuf-mode-hook #'display-line-numbers-mode)

(provide 'text/protobuf)
