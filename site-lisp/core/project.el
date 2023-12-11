(progn
  (with-eval-after-load 'project
	(defun project-switch-project (dir)
	  "Override default `project-switch-project' command.
Prohibit command prompt on `project-switch-project', instead directly execute `project-find-file'."
      (interactive (list (project-prompt-project-dir)))
      (let ((project-current-directory-override dir))
		(call-interactively 'project-find-file)))

	(defun core/project-root ()
      (or (ignore-errors (project-root (project-current)))
		  default-directory)))

  (require 'project))

(provide 'core/project)
