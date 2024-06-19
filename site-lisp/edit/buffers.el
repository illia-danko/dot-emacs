(defun edit/switch-to-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (get-buffer-create "*scratch*")))
    (switch-to-buffer buf)
    (lisp-interaction-mode)))

(provide 'edit/buffers)
