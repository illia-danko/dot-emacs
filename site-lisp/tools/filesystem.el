(progn
  (defun tools/dired-system-open ()
    (interactive)
    (let ((file (dired-get-filename nil t))
          (cmd (pcase system-type
                 ('darwin "open")
                 (_ "xdg-open"))))
      (call-process cmd nil 0 nil file)))

  (with-eval-after-load 'dired
	(customize-set-variable 'dired-dwim-target t) ; act as two panes midnight commander file manager.
	(customize-set-variable 'dired-omit-files "^\\...+$") ; add hiden files (started with dot) to `dired-omit-mode'

	(add-hook 'dired-mode-hook #'dired-hide-details-mode) ; do not show details (owners, access bits, etc.)
	(add-hook 'dired-mode-hook #'dired-omit-mode) ; do not show pattern's files
	)

  (require 'dired))

(provide 'tools/filesystem)
