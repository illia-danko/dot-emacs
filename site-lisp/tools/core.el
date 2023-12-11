
(unless (display-graphic-p)
  (with-eval-after-load 'xclip
    (xclip-mode 1))

  (require 'xclip))

(provide 'tools/core)
