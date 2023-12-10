(progn
  (with-eval-after-load 'marginalia
    (customize-set-variable 'marginalia-align 'right)
    (marginalia-mode 1))

  (require 'marginalia))

(progn
  (with-eval-after-load 'vertico
    (customize-set-variable 'vertico-count 17)
    (customize-set-variable 'vertico-cycle nil)
    (customize-set-variable 'vertico-resize nil)
    (vertico-mode 1))

  (require 'vertico))

(provide 'completion/minibuffer)
