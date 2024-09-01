;;;  bury-window.el --- Close predefined a buffers when keyboard-quit command (C-g) is trigger  -*- lexical-binding:t; -*-
(require 'eldoc)
(require 'help)

(defvar tools/bury-window-buffer-list
  '("*eldoc" "*Help*")
  "Buffer patterns ignored by `tools/bury-window-maybe-bury'.")

(defun tools/bury-window-but-this ()
  "Return window list excluding the current active one."
  (let* ((current-window (selected-window))
         (other-windows (seq-filter (lambda (win)
                                      (not (eq win current-window)))
                                    (window-list))))
    other-windows))

;;;###autoload
(defun tools/bury-window-maybe-bury ()
  "Close active windows satisfying `tools/bury-window-buffer-list'."
  (interactive)
  (mapc (lambda (win)
          (let ((buf-name (buffer-name (window-buffer win))))
            (and (string-match-p "^\\*.*\\*$" buf-name) ; leading and trailing asterisk match
                 (cl-remove-if
				  (lambda (str)
					(string-prefix-p str buf-name))
				  tools/bury-window-buffer-list)
                 (delete-window win))))
        (tools/bury-window-but-this)))

(advice-add 'keyboard-quit :before #'tools/bury-window-maybe-bury)

(provide 'tool/bury-window)

;;; bury-window.el ends here
