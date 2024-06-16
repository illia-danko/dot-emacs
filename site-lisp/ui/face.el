(require 'spacemacs-theme)
(require 'orderless)
(require 'completion)

(defvar ui/theme-dark-variant 'spacemacs-dark)
(defvar ui/theme-light-variant 'spacemacs-light)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font Bold 10"))

(defun ui/current-theme () (car custom-enabled-themes))
(defun ui/reload-theme () (load-theme (ui/current-theme) t))

(defun ui/customaize-theme (&optional frame)
  (interactive)
  (let ((frame (or frame (selected-frame))))
	(ui/customize-theme-frame frame)))

(defun ui/customize-theme-frame (frame)
  ;; Fix highlighting discrepancy.
  (mapc (lambda (face-group)
		  (let ((face (car face-group))
				(face-ref (cdr face-group)))
			(set-face-attribute face frame
								:background (face-background face-ref frame)
								:foreground (face-foreground face-ref frame)
								:weight (face-attribute face-ref :weight frame))))
		[(completions-common-part . orderless-match-face-0)
		 (completions-first-difference . orderless-match-face-1)]))

(defun ui/make-frame-hook ()
  (ui/reload-theme) ; fix corrupted colors scheme when frame is created
  (ui/customize-theme-frame nil))

(add-hook 'server-after-make-frame-hook #'ui/make-frame-hook)

(when core/use-no-extras
  (load-theme ui/theme-light-variant t))

(provide 'ui/face)
