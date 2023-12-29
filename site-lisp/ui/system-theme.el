;; Macos auto theme hook.
(require 'ui/face)
(require 'filenotify)

(unless (and (display-graphic-p) (eq system-type 'darwin))
  (defconst ui/theme-variant-filename "~/.config/appearance/background"
    "Current system `light' or `dark' theme. Tupically a theme variant set by a
  custom poll mechanism, for instance, iterm2 can provide auto-dark.py script
  which monitors global system theme
  (https://gist.github.com/FradSer/de1ca0989a9d615bd15dc6eaf712eb93) and fully
  under the user responsibilities.")

  (defun ui/load-theme-variant (filename)
    "Load variant theme from a file."
    (string-trim
     (with-temp-buffer
       (insert-file-contents filename)
       (buffer-string))))

  (defun ui/load-theme (&rest args)
    "Load `light' or `dark' theme variant."
    (let ((theme (pcase (ui/load-theme-variant ui/theme-variant-filename)
                   ("light" ui/theme-light-variant)
                   (_ ui/theme-dark-variant))))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)))

  (ui/load-theme)
  (ui/load-custom-faces)

  (file-notify-add-watch
   ui/theme-variant-filename
   '(change)
   #'ui/load-theme))

;; Macos https://github.com/d12frosted/homebrew-emacs-plus integration.
(when (and (display-graphic-p) (eq system-type 'darwin))
  (defun ui/apply-theme-macos-gui (appearance)
    "Load theme based on the system theme's variant."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
	  ('light (load-theme ui/theme-light-variant t))
      ('dark (load-theme ui/theme-dark-variant t)))
    (ui/load-custom-faces))

  (add-hook 'ns-system-appearance-change-functions #'ui/apply-theme-macos-gui))

(provide 'ui/system-theme)
