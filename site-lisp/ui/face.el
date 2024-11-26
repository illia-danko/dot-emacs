(require 'catppuccin-theme)

(defvar ui/theme-dark-variant 'catppuccin)
(defvar ui/theme-light-variant 'catppuccin)

(when core/use-no-extras
  (load-theme ui/theme-light-variant t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font Bold 10"))

(defun ui/customaize-theme (&optional frame)
  (interactive)
  (let ((frame (or frame (selected-frame))))
	(ui/customize-theme-frame frame)))

(defun ui/customize-theme-frame (frame)
  ;; Adjust border.
  (let ((display-table (or standard-display-table (make-display-table))))
	(set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
	(setq standard-display-table display-table))
  (set-face-attribute 'vertical-border frame
					  :background (face-background 'default))

  ;; Adjust orderless completion.
  (mapc (lambda (face-group)
		  (let ((face (car face-group))
				(face-ref (cdr face-group)))
			(set-face-attribute face frame
								:background (face-background face-ref frame)
								:foreground (face-foreground face-ref frame)
								:weight (face-attribute face-ref :weight frame))))
		[(completions-common-part . orderless-match-face-0)
		 (completions-first-difference . orderless-match-face-1)]))

(advice-add 'load-theme :around #'(lambda (orig-fun &rest args)
									(mapc #'disable-theme custom-enabled-themes)
									(apply orig-fun args)
									(ui/customize-theme-frame nil)))

(defun ui/current-theme () (car custom-enabled-themes))
(defun ui/reload-theme () (load-theme (ui/current-theme) t)) ; fix corrupted colors scheme when frame is created
(add-hook 'server-after-make-frame-hook #'ui/reload-theme)

(provide 'ui/face)
