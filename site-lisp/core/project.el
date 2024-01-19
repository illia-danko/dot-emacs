(require 'project)

;; Uses in conjunction with `project-prefix-map' mapping.
(add-to-list 'project-switch-commands '(vterm "VTerm"))
(add-to-list 'project-switch-commands '(tool/magit-status "Magit Status"))
(add-to-list 'project-switch-commands '(rg "RipGrep"))

(defun core/project-root ()
  (or (ignore-errors (project-root (project-current)))
	  default-directory))

(provide 'core/project)
