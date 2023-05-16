;; Start Emacs maximized, use custom font.
(when window-system
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :family "JetBrainsMono Nerd Font Mono"
                              :weight 'semibold
                              :height (or (and (eq system-type 'darwin) 150) 120)))
        [default variable-pitch fixed-pitch fixed-pitch-serif])
  (toggle-frame-maximized))

(defun my-adjust-faces ()
  (let ((display-table (or standard-display-table (make-display-table))))
	(set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
	(setq standard-display-table display-table))
  (set-face-attribute 'vertical-border nil
                      :background (face-background 'default)))

(use-package doom-themes :straight t)
(use-package spacemacs-theme :straight t :defer t
  :custom
  (spacemacs-theme-comment-bg nil))

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
                   ("light" 'spacemacs-light)
                   (_ 'doom-one))))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)))

  (my-load-theme)
  (my-adjust-faces)

  (require 'filenotify)
  (file-notify-add-watch
   my-theme-filename
   '(change)
   #'my-load-theme))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

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
    ('light (load-theme 'spacemacs-light t))
    ('dark (load-theme 'one-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme-ns)
(add-hook 'after-load-theme-hook #'my-adjust-faces)

(provide 'init-appearance)
