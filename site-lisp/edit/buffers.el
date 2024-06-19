(defun edit/switch-to-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (get-buffer-create "*scratch*")))
    (switch-to-buffer buf)
    (lisp-interaction-mode)))

(defun edit/switch-to-messages-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Messages*")))
    (switch-to-buffer buf)))

(provide 'edit/buffers)
