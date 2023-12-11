(progn
  (defun core/project-root ()
    (or (ignore-errors (project-root (project-current)))
	default-directory))

  (require 'project))

(provide 'core/project)
