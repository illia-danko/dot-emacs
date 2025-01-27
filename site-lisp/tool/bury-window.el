;;;  bury-window.el --- Close predefined a buffers when keyboard-quit command (C-g) is trigger  -*- lexical-binding:t; -*-
(defvar tools/bury-window-buffer-list
  '("*eldoc" "*help*" "*embark" "magit-revision")
  "Buffer patterns ignored by `tools/bury-window-maybe-bury'.")

(defun tools/bury-other-window-list ()
  "Return window list excluding the current active one."
  (let* ((current-window (selected-window))
         (other-windows (seq-filter (lambda (win)
                                      (not (eq win current-window)))
                                    (window-list))))
    other-windows))

;;;###autoload
(defun tools/bury-window-maybe-bury ()
  "Close active windows satisfying any of `tools/bury-window-buffer-list' prefix patterns"
  (interactive)
  (mapc (lambda (win)
          (let* ((wbuf (window-buffer win))
				 (buf-name (buffer-name wbuf))
				 (buf (downcase buf-name)))
			(and (cl-remove-if-not
				  (lambda (str)
					(string-prefix-p str buf))
				  tools/bury-window-buffer-list)
				 (delete-window win))))
		(tools/bury-other-window-list)))

(advice-add 'keyboard-quit :before #'tools/bury-window-maybe-bury)

(provide 'tool/bury-window)

;;; bury-window.el ends here
