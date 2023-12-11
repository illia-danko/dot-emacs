(when window-system
  (mapc (lambda (face)
          (set-face-attribute face nil
							  :weight 'bold
							  :family (or (and (eq system-type 'darwin) "JetBrainsMono Nerd Font") "IosevkaTerm Nerd Font Mono")
							  :height (or (and (eq system-type 'darwin) 135) 120)))
        [default variable-pitch fixed-pitch fixed-pitch-serif]))

(provide 'ui/font)
