(require 'dashboard)
(require 'api/macro)

(api/customize-set-variable*
 'dashboard-filter-agenda-entry 'dashboard-no-filter-agenda ; show todo entries
 'dashboard-items '((agenda . 16)) ; layout
 'dashboard-projects-backend 'project-el ; use project-el as project backend
 'dashboard-set-footer nil  ; do not display footer
 'dashboard-vertically-center-content t ; helps to remove wiered last line highlight
 )

;; `daemonp' is used to ensure that there is not an emtpy dashboard buffer is created when starts Emacs.
(when (daemonp)
  (api/customize-set-variable*
   'initial-buffer-choice #'(lambda () (get-buffer-create dashboard-buffer-name))))

(dashboard-setup-startup-hook)

(defun extras/dashboard-refresh-buffer (&rest _)
  (interactive)
  (let ((dashboard-force-refresh t))
	(dashboard-insert-startupify-lists)))

(add-hook 'server-after-make-frame-hook #'extras/dashboard-refresh-buffer)

(provide 'extras/dashboard)
