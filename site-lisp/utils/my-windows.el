(defun my-windows-but-this ()
  (let* ((current-window (selected-window))
         (other-windows (seq-filter (lambda (win)
                                      (not (eq win current-window)))
                                    (window-list))))
    other-windows))

(defun my-close-virtual-windows ()
  (interactive)
  (mapc (lambda (win)
          (let ((buf-name (buffer-name (window-buffer win))))
            (and (string-prefix-p "*" buf-name)
                 (string-suffix-p "*" buf-name)
                 (delete-window win))))
        (my-windows-but-this)))

(defun my-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (my-close-virtual-windows)
  (keyboard-quit))

(global-set-key (kbd "C-g") 'my-keyboard-quit)

(provide 'my-windows)
