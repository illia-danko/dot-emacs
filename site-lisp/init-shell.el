(defun my-exec-shell (input-prefix buffer-name &optional command-prefix indent-p mode)
  (let* ((cmd (read-string input-prefix))
         (buf (get-buffer-create buffer-name))
         (command-prefix (if command-prefix (concat command-prefix " ") "")))
    (with-current-buffer buf
      (erase-buffer)
      (shell-command (concat command-prefix cmd) buf)
      (and mode
           (funcall (symbol-function mode)))
      (and indent-p
           (save-excursion
             (mark-whole-buffer)
             (indent-region (point-min) (point-max))
             (deactivate-mark)))
      (pop-to-buffer-same-window buf))))

(defun my-exec-shell-generic ()
  (interactive)
  (my-exec-shell "Cmd: " "*exec-shell-cmd*"))

(global-set-key (kbd "C-c ee") #'my-exec-shell-generic)

(defun my-exec-shell-curl ()
  "Reads url, get html via external curl command and returns result to
 *web-query* buffer.

Curl args:
-L - to follow redirections.
-s - Silent mode. Do not show progress meter or error messages."
  (interactive)
  (require 'sgml-mode)
  (my-exec-shell "URL: " "*exec-shell-curl*" "curl -L -s" t 'html-mode))

(global-set-key (kbd "C-c ec") #'my-exec-shell-curl)

(provide 'init-shell)
