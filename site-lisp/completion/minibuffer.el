(require 'nerd-icons-completion)
(require 'marginalia)
(require 'vertico)
(require 'vertico-posframe)
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
 'vertico-multiform-commands '((consult-line
								posframe
								(vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
								(vertico-posframe-border-width . 10)
								;; NOTE: This is useful when emacs is used in both in X and
								;; terminal, for posframe do not work well in terminal, so
								;; vertico-buffer-mode will be used as fallback at the
								;; moment.
								(vertico-posframe-fallback-mode . vertico-buffer-mode))
							   (t posframe))
 ;; Add margin to posframe buffer.
 'vertico-posframe-parameters '((left-fringe . 8)
								(right-fringe . 8)))

(vertico-mode 1)
(vertico-posframe-mode 1)
(vertico-multiform-mode 1)

;; HACK(idanko): posframe loose the border. Refresh it.
(add-hook 'magit-post-refresh-hook #'posframe-delete-all)

(provide 'completion/minibuffer)
