(require 'emacs)

(defvar ui/theme-dark-variant 'modus-vivendi)
(defvar ui/theme-light-variant 'modus-operandi)

(defun ui/current-theme ()
  (car custom-enabled-themes))

(defun ui/customize-font (&optional frame)
  (interactive)

  (when (display-graphic-p)
	(let ((frame (or frame (selected-frame))))
	  (mapc (lambda (face)
			  (set-face-attribute face frame
								  ;; :weight 'bold
								  :family (or (and (eq system-type 'darwin) "JetBrainsMono Nerd Font") "JetBrainsMono Nerd Font Mono")
								  :height (or (and (eq system-type 'darwin) 135) 105)))
			[default variable-pitch fixed-pitch fixed-pitch-serif]))))

(defun ui/customaize-theme (&optional frame)
  (interactive)

  (ui/customize-font frame)

  (let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
    (setq standard-display-table display-table))
  (set-face-attribute 'vertical-border frame
					  :background (face-background 'default))

  (when (featurep 'vterm)
    ;; Fix zsh-autosuggestions highlight color problem from One Themes.
    (if (eq (ui/current-theme) ui/theme-light-variant)
		(mapc (lambda (face)
				(set-face-attribute face frame :background "#a0a1a7"))
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

(add-hook 'after-make-frame-functions #'ui/customaize-theme)

(defvar ui/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after ui/run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'ui/after-load-theme-hook))

(add-hook 'ui/after-load-theme-hook #'ui/customaize-theme)

;; Open window maximized.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'ui/face)
