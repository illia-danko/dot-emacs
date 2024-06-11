(require 'emacs)
(require 'doom-themes)

(defvar ui/theme-dark-variant 'doom-one)
(defvar ui/theme-light-variant 'doom-one-light)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font Bold 10"))

(defun ui/current-theme ()
  (car custom-enabled-themes))

(defun ui/customaize-theme (&optional frame)
  (interactive)

  (let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
    (setq standard-display-table display-table))
  (set-face-attribute 'vertical-border frame
					  :background (face-background 'default))

  (when (featurep 'vterm)
    ;; Fix zsh-autosuggestions highlight color problem from One Themes.
    (if (eq (ui/current-theme) ui/theme-light-variant)
		(mapc (lambda (face)
				(set-face-attribute face nil :background "#a0a1a7"))
			  [term-color-black vterm-color-black])))

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


(defun ui/customize-theme-wrap (&optional frame)
  (run-with-timer 1 nil #'ui/customaize-theme frame))

(add-hook 'after-make-frame-functions #'ui/customize-theme-wrap)

(defvar ui/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after ui/run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'ui/after-load-theme-hook))

(add-hook 'ui/after-load-theme-hook #'ui/customaize-theme)

(provide 'ui/face)
