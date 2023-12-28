(require 'flyspell)
(require 'ispell)

(defun tool/spelling-toggle-buffer ()
  (interactive)
  (if (symbol-value flyspell-mode)
	  (progn
		(message "Flyspell off")
		(flyspell-mode -1))
	(progn
	  (message "Flyspell on")
	  (if (derived-mode-p 'prog-mode)
		  (flyspell-prog-mode)
		(flyspell-mode))
	  (flyspell-buffer))))


(provide 'tool/spelling)
