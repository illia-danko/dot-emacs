;; Start Emacs maximized, use custom font.
(when window-system
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :family "JetBrainsMono Nerd Font Mono"
                              :weight 'semibold
                              :height 125))
        [default variable-pitch fixed-pitch fixed-pitch-serif])
  (toggle-frame-maximized))

(use-package spacemacs-theme :straight t :defer t
  :custom
  (spacemacs-theme-comment-bg nil))

(when (eq system-type 'gnu/linux)
  (defun my-apply-theme (&optional frame)
	"Adjust faces."
	(when frame
      (select-frame frame))
	(mapc #'disable-theme custom-enabled-themes)
	(load-theme 'spacemacs-dark t)
    ;; Fix terminal vertical-border glyph.
    ;; (https://emacs.stackexchange.com/questions/7228/nice-tty-window-borders-in-24-4).
    (let ((display-table (or standard-display-table (make-display-table))))
	  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
	  (setq standard-display-table display-table))
    ;; Make a vertical border as a tmux' one.
    (set-face-attribute 'vertical-border nil
                        :background (face-background 'default)
                        :foreground (face-background 'hl-line))
    (set-face-attribute 'line-number nil
                        :background (face-background 'default))
    (set-face-attribute 'completions-common-part nil
                        :background (face-background 'orderless-match-face-0 frame)
                        :foreground (face-foreground 'orderless-match-face-0 frame)
                        :weight (face-attribute 'orderless-match-face-0 :weight frame))
    (set-face-attribute 'completions-first-difference nil
                        :background (face-background 'orderless-match-face-1 frame)
                        :foreground (face-foreground 'orderless-match-face-1 frame)
                        :weight (face-attribute 'orderless-match-face-1 :weight frame)))
  (my-apply-theme)
  (add-hook 'after-make-frame-functions #'my-apply-theme)
  (add-hook 'after-init-hook #'my-apply-theme))

;; Fancy modeline theme.
(use-package doom-modeline :straight t
  :custom
  (doom-modeline-height 36)
  :config
  (doom-modeline-mode 1))

;; Macos auto theme hook.
(defun my-apply-theme-ns (appearance)
  "Load theme based on the system theme's variant."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'spacemacs-light t))
    ('dark (load-theme 'spacemacs-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme-ns)

(provide 'init-appearance)
