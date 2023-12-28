;; NOTE: After adding treesitter to the mode the manual interaction required: M-x
;; edit/treesit-install-language-grammar RET.

(require 'treesit)

(defun edit/treesit-install-language-grammar ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))


(provide 'edit/treesit)
