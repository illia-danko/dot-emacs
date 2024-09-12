(require 'xclip)
(require 'compile)

;; xclip.
(unless (display-graphic-p)
  (xclip-mode 1))

 (provide 'tool/core)
