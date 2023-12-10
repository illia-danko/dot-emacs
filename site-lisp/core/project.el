(progn
  (with-eval-after-load 'project
    (defun my-project-root ()
      (or (ignore-errors (project-root (project-current)))
	  default-directory)))

  (require 'project))

(provide 'core/project)
