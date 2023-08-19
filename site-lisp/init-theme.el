(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; Start Emacs maximized, use custom font.
(when window-system
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :family "JetBrainsMono Nerd Font Mono"
                              :weight 'bold
                              :height (or (and (eq system-type 'darwin) 140) 120)))
        [default variable-pitch fixed-pitch fixed-pitch-serif])
  (toggle-frame-maximized))

(defun my-current-theme ()
  (car custom-enabled-themes))

(defun my-adjust-faces (&optional frame)
  (let ((display-table (or standard-display-table (make-display-table))))
	(set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
	(setq standard-display-table display-table))
  (set-face-attribute 'vertical-border frame
                      :background (face-background 'default)))


(add-hook 'after-load-theme-hook #'my-adjust-faces)
(add-hook 'after-make-frame-functions #'my-adjust-faces)

(use-package doom-themes :straight t)

(unless (display-graphic-p)
  (defconst my-theme-filename "~/.config/appearance/background"
    "Current system `light' or `dark' theme. Tupically a theme variant set by a
  custom poll mechanism, for instance, iterm2 can provide auto-dark.py script
  which monitors global system theme
  (https://gist.github.com/FradSer/de1ca0989a9d615bd15dc6eaf712eb93) and fully
  under the user responsibilities.")

  (defun my-load-background-theme (filename)
    (string-trim
     (with-temp-buffer
       (insert-file-contents filename)
       (buffer-string))))

  (defun my-load-theme (&rest args)
    (let ((theme (pcase (my-load-background-theme my-theme-filename)
                   ("light" 'modus-operandi-deuteranopia)
                   (_ 'modus-vivendi-tinted))))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)))

  (my-load-theme)
  (my-adjust-faces)

  (require 'filenotify)
  (file-notify-add-watch
   my-theme-filename
   '(change)
   #'my-load-theme))

;; Fancy modeline theme.
(use-package doom-modeline :straight t
  :custom
  (doom-modeline-icon nil)

  :config
  (doom-modeline-mode 1))


;; Macos auto theme hook.
(defun my-apply-theme-ns (appearance)
  "Load theme based on the system theme's variant."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi-deuteranopia t))
    ('dark (load-theme 'modus-vivendi-tinted t)))
  (my-adjust-faces))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme-ns)

(provide 'init-theme)
