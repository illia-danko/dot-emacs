(require 'api/window)

(with-eval-after-load 'compile
  (defvar-local tool/before-compilation-frame-win-number nil
    "Number of opened frame windows before compilation run.")

  (defun tool/save-compile-window-number (&rest args)
    (setq-local tool/before-compilation-frame-win-number (api/window-number)))

  (defun tool/bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings."
    (when (and
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      ;; Close the compilation window if the frame layout has
                      ;; been changed, otherwise `switch-buffer'.
                      (lambda (buf)
                        (bury-buffer buf)
                        (if (= tool/before-compilation-frame-win-number
                               (api/window-number))
                            (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                          (delete-windows-on buf)))
                      buffer)))

  (advice-add 'compilation-start :before 'tool/save-compile-window-number)
  (add-hook 'compilation-finish-functions #'tool/bury-compile-buffer-if-successful))

(provide 'tool/autoclose-compile-window)
