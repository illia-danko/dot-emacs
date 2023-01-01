;;; zentoggle.el --- Destruction Free Functions -*- lexical-binding: t; -*-

;;;###autoload

;; TODO(idanko): check if modes are enabled.
(defun zen-toggle (&optional arg)
  (interactive)
  (let* ((toggle-l (if olivetti-mode 1 -1))
         (toggle-z (if olivetti-mode -1 1)))
    (display-line-numbers-mode toggle-l)
    (olivetti-mode toggle-z)
    (hide-mode-line-mode toggle-z)
    (git-gutter-mode toggle-l)))

(provide 'zentoggle)

;;; zentoggle.el ends here
