(defvar core/intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode core/intercept-mode
  "Global high precedence minor mode."
  :global t)

(core/intercept-mode)

(provide 'core/intercept-mode)
