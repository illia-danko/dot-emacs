(defun api/window-number ()
  (length (mapcar #'window-buffer (window-list))))

(provide 'api/window)
