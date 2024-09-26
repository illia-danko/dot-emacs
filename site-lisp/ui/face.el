(require 'doom-themes)
(require 'orderless)
(require 'completion)

(defvar ui/theme-dark-variant 'doom-one)
(defvar ui/theme-light-variant 'doom-one-light)

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
  ;; Fix highlighting discrepancy.
  (let ((display-table (or standard-display-table (make-display-table))))
	(set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
	(setq standard-display-table display-table))

  (set-face-attribute 'vertical-border frame
					  :background (face-background 'default)))

(advice-add 'load-theme :around #'(lambda (orig-fun &rest args)
									(mapc #'disable-theme custom-enabled-themes)
									(apply orig-fun args)
									(ui/customize-theme-frame nil)))

(defun ui/current-theme () (car custom-enabled-themes))
(defun ui/reload-theme () (load-theme (ui/current-theme) t)) ; fix corrupted colors scheme when frame is created
(add-hook 'server-after-make-frame-hook #'ui/reload-theme)

(provide 'ui/face)
