;; NOTE: After adding treesitter to the mode the manual interaction required: M-x
;; edit/treesit-install-language-grammar RET.

(progn
  (with-eval-after-load 'treesit
	(defun edit/treesit-install-language-grammar ()
	  (interactive)
	  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
	)

  (require 'treesit))

(provide 'edit/treesit)
