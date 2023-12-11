(require 'core/core)

(defun tool/git-push-current-file (pattern)
  "Stage, commit and push to upstream a personal org note file."
  (interactive)
  (let* ((fullname (buffer-file-name))
         (relname (file-name-nondirectory fullname))
         (current-project (core/project-root)))
    (if (string-prefix-p (expand-file-name pattern) fullname)
        (progn
          (call-process "git" nil nil nil "add" fullname)
          (call-process "git" nil nil nil "commit" "-m" (format "Update %s" relname))
          (call-process "git" nil nil nil "push")
          (message "Pushed %s" relname))
      (message "%S not a part of %S" fullname pattern))))

(progn
  (with-eval-after-load 'magit
	(customize-set-variable 'magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1) ; magit uses the whole frame space
	(customize-set-variable 'magit-diff-refine-hunk 'all)  ; word-wise diff highlight
	)

  ;; Eager loading.
  (require 'magit))

(progn
  (with-eval-after-load 'git-link
	(defun tool/browse-project-home-page ()
      (interactive)
      (let ((git-link-open-in-browser t))
		(call-interactively 'git-link-homepage))))

  (require 'git-link))

(provide 'tool/version-control)
