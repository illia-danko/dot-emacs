(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; Start Emacs maximized, use custom font.
(when window-system
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :weight 'bold
                              :family (or (and (eq system-type 'darwin) "JetBrainsMono Nerd Font") "IosevkaTerm Nerd Font Mono")
                              :height (or (and (eq system-type 'darwin) 150) 120)))
        [default variable-pitch fixed-pitch fixed-pitch-serif])
  (toggle-frame-maximized))

(defun my-current-theme ()
  (car custom-enabled-themes))

(defun my-adjust-faces (&optional frame)
  (let ((display-table (or standard-display-table (make-display-table))))
	(set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
	(setq standard-display-table display-table))
  (set-face-attribute 'vertical-border frame
                      :background (face-background 'default))

  ;; Fix zsh-autosuggestions highlight color problem from One Themes.
  (if (eq (my-current-theme) 'doom-one-light)
      (mapc (lambda (face)
              (set-face-attribute face frame :background "#a0a1a7"))
            [term-color-black vterm-color-black]))

  ;; Fix discrepancy between match highlighting.
  (mapc (lambda (face-group)
          (let ((face (car face-group))
                (face-ref (cdr face-group)))
            (set-face-attribute face frame
                                :background (face-background face-ref frame)
                                :foreground (face-foreground face-ref frame)
                                :weight (face-attribute face-ref :weight frame))))
        [(completions-common-part . orderless-match-face-0)
         (completions-first-difference . orderless-match-face-1)]))

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
                   ("light" 'doom-one-light)
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

;; Fancy modeline theme.
(use-package doom-modeline :straight t
  :custom
  (doom-modeline-icon t)
  :config
  (doom-modeline-mode 1))

;; Macos auto theme hook.
(defun my-apply-theme-ns (appearance)
  "Load theme based on the system theme's variant."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-one-light t))
    ('dark (load-theme 'doom-one t)))
  (my-adjust-faces))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme-ns)

(provide 'init-theme)
