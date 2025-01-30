(require 'nerd-icons-completion)
(require 'marginalia)
(require 'vertico)
(require 'api/macro)

;; marginalia.
(api/customize-set-variable* 'marginalia-align 'right)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
(marginalia-mode 1)

;; vertico.
(api/customize-set-variable*
 'vertico-count 21
 'vertico-cycle nil
 'vertico-resize nil
 'vertico-buffer-display-action '(display-buffer-full-frame))

(vertico-mode 1)
(vertico-buffer-mode 1)

(provide 'completion/minibuffer)
