(require 'wgrep)
(require 'rg)
(require 'core/project)
(require 'api/macro)

(api/customize-set-variable* 'wgrep-auto-save-buffer t  ; automatically save buffers after edit
							 'rg-required-command-line-flags
							 (append '("--color=always"
									   "--colors=match:fg:red"
									   "--colors=path:fg:magenta"
									   "--colors=line:fg:green"
									   "--colors=column:none"
									   "-n")
									 (string-split (getenv "RG_OPTS_FILTER"))))

(defun tool/rg-project (&optional args)
  "Run vterm under the project root folder."
  (interactive)
  (let ((default-directory (core/project-root)))
	(call-interactively 'rg)))

(provide 'tool/rg)
