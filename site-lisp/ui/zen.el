(progn
  (with-eval-after-load 'olivetti
	(defun ui/zen-toggle ()
	  (interactive)
	  (call-interactively 'olivetti-mode)))

  (require 'olivetti))

(require 'hide-mode-line)

(provide 'ui/zen)
