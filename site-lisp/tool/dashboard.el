(require 'dashboard)
(require 'api/variable)

(api/customize-set-variable*
 'initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*"))
 'dashboard-filter-agenda-entry 'dashboard-no-filter-agenda ; show todo entries
 'dashboard-items '((agenda . 16)) ; layout
 'dashboard-projects-backend 'project-el ; use project-el as project backend
 'dashboard-set-footer nil) ; do not display footer

(dashboard-setup-startup-hook)

(provide 'tool/dashboard)
