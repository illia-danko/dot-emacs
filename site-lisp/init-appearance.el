;; Start Emacs maximized, use custom font.
(when window-system
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :family "JetBrainsMono Nerd Font Mono"
                              :weight 'semibold
                              :height (or (and (eq system-type 'darwin) 150) 120)))
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
    (load-theme 'spacemacs-light t))

  (my-apply-theme)
  (add-hook 'after-make-frame-functions #'my-apply-theme)
  (add-hook 'after-init-hook #'my-apply-theme))

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
    ('dark (load-theme 'spacemacs-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme-ns)

(provide 'init-appearance)
