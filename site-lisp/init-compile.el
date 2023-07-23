(use-package compile
  :init
  (defvar-local my-before-compilation-frame-win-number nil
    "Number of opened frame windows before compilation run.")

  (defun my-window-number ()
    (length (mapcar #'window-buffer (window-list))))

  (defun my-save-window-number (&rest args)
    (setq-local my-before-compilation-frame-win-number (my-window-number)))

  (defun my-bury-compile-buffer-if-successful (buffer string)
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
                        (if (= my-before-compilation-frame-win-number
                               (my-window-number))
                            (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                          (delete-windows-on buf)))
                      buffer)))

  (defun my-project-compile (arg)
    "If universal argument is provided (C-u) then supplies compile
command prompt. Otherwise recompile."
    (interactive "P")
    (if arg
	    (project-compile)
	  (recompile)))

  :config
  (advice-add 'compilation-start :before 'my-save-window-number)
  (add-hook 'compilation-finish-functions #'my-bury-compile-buffer-if-successful)

  :bind
  ("C-c b" . my-project-compile))


;; Pretty print term escape codes on compilation buffer.
(use-package ansi-color
  :init
  (defun my-ansi-color-compilation-buffer ()
    "Apply ANSI color codes to the compilation buffer."
    (toggle-read-only)  ; Disable read-only mode temporarily
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only)) ; Re-enable read-only mode
  :hook (compilation-filter . my-ansi-color-compilation-buffer))

(provide 'init-compile)
