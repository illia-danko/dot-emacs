(when (display-graphic-p)
  (with-eval-after-load 'all-the-icons-completion
    (all-the-icons-completion-mode 1))

  (require 'all-the-icons-completion))

(provide 'ui/icons)
