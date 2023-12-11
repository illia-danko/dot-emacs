(progn
  (with-eval-after-load 'dashboard
    (add-hook 'after-make-frame-functions
	      (lambda (&optional frame)
		(setq initial-buffer-choice (lambda nil
					      (get-buffer "*dashboard*")))))

    (customize-set-variable 'dashboard-filter-agenda-entry 'dashboard-no-filter-agenda) ; show todo entries
    (customize-set-variable 'dashboard-items '((agenda . 16))) ; layout
    (customize-set-variable 'dashboard-projects-backend 'project-el) ; use project-el as project backend
    (customize-set-variable 'dashboard-set-footer nil) ; do not display footer
    (dashboard-setup-startup-hook))

  (require 'dashboard))

(provide 'tools/dashboard)
