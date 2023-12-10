(defvar ui/bury-window-buffers-whitelist
  '("*dashboard*" "*compilation*" "*embark export")
  "Buffer patterns ignored by `ui/bury-window-maybe-bury'.")

(defun ui/bury-window-but-this ()
  (let* ((current-window (selected-window))
         (other-windows (seq-filter (lambda (win)
                                      (not (eq win current-window)))
                                    (window-list))))
    other-windows))

(defun ui/bury-window-maybe-bury ()
  (interactive)
  (mapc (lambda (win)
          (let ((buf-name (downcase (buffer-name (window-buffer win)))))
            (and (string-prefix-p "*" buf-name)
                 (string-suffix-p "*" buf-name)
                 (not (cl-remove-if-not
                       (lambda (str)
                         (string-prefix-p str buf-name))
                       ui/bury-window-buffers-whitelist))
                 (delete-window win))))
        (ui/bury-window-but-this)))

;; TODO(idanko): add cookie
(defun ui/bury-window-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (ui/bury-window-maybe-bury)
  (keyboard-quit))

;; (global-set-key (kbd "C-g") 'my-keyboard-quit)

(provide 'ui/bury-window)
