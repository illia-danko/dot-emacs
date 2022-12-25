;;; zentoggle.el --- Destruction Free Functions -*- lexical-binding: t; -*-

;;;###autoload

(defun zen-toggle (&optional arg)
  (interactive)
  (let* ((toggle-l (if olivetti-mode 1 -1))
         (toggle-z (if olivetti-mode -1 1)))
    (display-line-numbers-mode toggle-l)
    (olivetti-mode toggle-z)
    (hide-mode-line-mode toggle-z)))

(provide 'zentoggle)

;;; zentoggle.el ends here
