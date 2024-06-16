(require 'olivetti)
(require 'hide-mode-line)
(require 'display-line-numbers)
(require 'git-gutter)

(defun ui/zen-toggle ()
  (interactive)
  (let* ((enabled-p (not olivetti-mode))
		 (numbers-p display-line-numbers)
		 (hide-mode-line-p hide-mode-line-mode)
		 (gutter-p git-gutter-mode))
	(call-interactively 'olivetti-mode enabled-p)

	(when enabled-p
	  (setq-local ui/zen-buffer-status `(:enabled ,enabled-p
												  :numbers ,numbers-p
												  :hide-mode-line ,hide-mode-line-p
												  :gutter ,gutter-p))
	  (and numbers-p (call-interactively 'display-line-numbers-mode nil))
	  (or hide-mode-line-p (call-interactively 'hide-mode-line-mode t))
	  (and gutter-p (call-interactively 'git-gutter-mode nil)))

	(unless enabled-p
	  (when (plist-get ui/zen-buffer-status :gutter)
		(call-interactively 'git-gutter-mode t))
	  (unless (plist-get ui/zen-buffer-status :hide-mode-line)
		(call-interactively 'hide-mode-line-mode nil))
	  (when (plist-get ui/zen-buffer-status :numbers)
		(call-interactively 'display-line-numbers-mode t)))))


(provide 'ui/zen)
