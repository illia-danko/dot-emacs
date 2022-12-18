;;; zentoggle.el --- Destruction Free Functions -*- lexical-binding: t; -*-

;;;###autoload
(defun zen-toggle (&optional arg)
  (interactive)
  (call-interactively 'olivetti-mode arg)
  (call-interactively 'hide-mode-line-mode arg))

(provide 'zentoggle)

;;; zentoggle.el ends here
