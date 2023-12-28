(require 'core/core)
(require 'tool/spelling)
(require 'api/variable)

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
	(api/customize-set-variable*
	 'magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ; magit uses the whole frame space
	 'magit-diff-refine-hunk 'all)  ; word-wise diff highlight
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

(progn
  (with-eval-after-load 'git-commit
	(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

  (require 'git-commit))

(progn
  (with-eval-after-load 'git-gutter-fringe
	(defun tool/vc-git-gutter-popup-hunk-jump (&optional diffinfo)
	  (interactive)
	  (git-gutter:popup-hunk diffinfo)
	  (switch-to-buffer-other-window git-gutter:popup-buffer))

	(mapc (lambda (fringe-face)
			(fringe-helper-define fringe-face nil
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"
			  "....XXXX"))
          [git-gutter-fr:added git-gutter-fr:deleted git-gutter-fr:modified])

	(api/customize-set-variable*
	 'git-gutter:ask-p nil
	 'git-gutter-fr:side 'right-fringe)

	(global-git-gutter-mode 1))

  (require 'git-gutter-fringe))

(provide 'tool/version-control)
