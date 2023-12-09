(defvar my-prettier-config-path (concat
                                 "--config="
                                 (expand-file-name "~/.config/prettier/prettier.config.js"))
  "Prettier config path.")

(defvar my-emacs-config-directory "~/.config/emacs"
  "Emacs configuration files path.")

(setq org-directory "~/codeberg.org/eli87/org")

;; Load private-settings.el.
(let ((private-settings (expand-file-name "private-settings.el" my-emacs-config-directory)))
  (and private-settings
       (file-exists-p private-settings)
       (load private-settings)))

(provide 'init-vars)
