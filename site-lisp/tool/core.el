(require 'xclip)
(require 'compile)
(require 'ttymux)

;; xclip.
(unless (display-graphic-p)
  (xclip-mode 1))

(unless (display-graphic-p)
  (ttymux-mode 1))

(provide 'tool/core)
