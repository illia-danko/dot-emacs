(require 'core/core)
(require 'nerd-icons-dired)
(require 'dired)
(require 'api/macro)

;; dired.
(defun tool/dired-system-open ()
  (interactive)
  (let ((file (dired-get-filename nil t))
        (cmd (pcase system-type
               ('darwin "open")
               (_ "xdg-open"))))
    (call-process cmd nil 0 nil file)))

(api/customize-set-variable*
 'dired-dwim-target t ; act as two panes midnight commander file manager.
 'dired-omit-files "^\\...+$") ; add hiden files (started with dot) to `dired-omit-mode'

(add-hook 'dired-mode-hook #'dired-hide-details-mode) ; do not show details (owners, access bits, etc.)
(add-hook 'dired-mode-hook #'dired-omit-mode) ; do not show pattern's files
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
(add-hook 'dired-mode-hook #'(lambda () (display-line-numbers-mode -1)))


(provide 'tool/dired)
