(require 'api/variable)

(progn
  (with-eval-after-load 'dashboard
    (add-hook 'after-make-frame-functions
			  (lambda (&optional frame)
				(setq initial-buffer-choice (lambda nil
											  (get-buffer "*dashboard*")))))

    (api/customize-set-variable*
	 'dashboard-filter-agenda-entry 'dashboard-no-filter-agenda ; show todo entries
     'dashboard-items '((agenda . 16)) ; layout
     'dashboard-projects-backend 'project-el ; use project-el as project backend
     'dashboard-set-footer nil) ; do not display footer

    (dashboard-setup-startup-hook))

  (require 'dashboard))

(provide 'tool/dashboard)
