(require 'api/variable)

(progn
  (with-eval-after-load 'marginalia
    (api/customize-set-variable* 'marginalia-align 'right)
    (marginalia-mode 1))

  (require 'marginalia))

(progn
  (with-eval-after-load 'vertico
    (api/customize-set-variable*
	 'vertico-count 17
	 'vertico-cycle nil
	 'vertico-resize nil)

    (vertico-mode 1))

  (require 'vertico))

(provide 'completion/minibuffer)
