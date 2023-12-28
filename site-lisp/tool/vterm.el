(require 'vterm)
(require 'core/project)

(defun tool/vterm-project (&optional args)
  (interactive)
  (let ((default-directory (core/project-root)))
	(vterm)))

(provide 'tool/vterm)
