(require 'emacs)
(require 'zerodark-theme)

(defvar ui/theme-dark-variant 'zerodark)
(defvar ui/theme-light-variant 'zerodark)

(defun ui/current-theme ()
  (car custom-enabled-themes))

(defun ui/load-font-faces (&optional frame)
  (mapc (lambda (face)
		  (set-face-attribute face frame
							  :weight 'bold
							  :family (or (and (eq system-type 'darwin) "JetBrainsMono Nerd Font") "JetBrainsMono Nerd Font Mono")
							  :height (or (and (eq system-type 'darwin) 135) 110)))
        [default variable-pitch fixed-pitch fixed-pitch-serif]))

(defun ui/load-custom-faces (&optional frame)
  (interactive)

  ;; Load core fonts.
  (ui/load-font-faces frame)

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

(defvar ui/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after ui/run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'ui/after-load-theme-hook))

(add-hook 'ui/after-load-theme-hook #'ui/load-custom-faces)
(add-hook 'after-make-frame-functions #'ui/load-custom-faces)

;; Open window maximized.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'ui/face)
