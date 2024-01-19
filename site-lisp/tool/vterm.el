(require 'vterm)
(require 'core/project)
(require 'api/macro)

(defun tool/vterm-project (&optional args)
  (interactive)
  (let ((default-directory (core/project-root)))
	(vterm)))

(api/customize-set-variable* 'vterm-max-scrollback 10000) ; terminal buffer lines number
(add-to-list 'vterm-keymap-exceptions "f1")
(add-hook 'vterm-mode-hook #'(lambda () (display-line-numbers-mode -1)))

(provide 'tool/vterm)
