(require 'wgrep)
(require 'rg)
(require 'core/project)
(require 'api/macro)

(api/customize-set-variable* 'wgrep-auto-save-buffer t) ; automatically save buffers after edit

(defun tool/rg-project (&optional args)
  "Run vterm under the project root folder."
  (interactive)
  (let ((default-directory (core/project-root)))
	(call-interactively 'rg)))

(provide 'tool/rg)
