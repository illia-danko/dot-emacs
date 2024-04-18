(require 'magit)
(require 'git-link)
(require 'git-commit)
(require 'git-gutter-fringe)

(require 'core/core)
(require 'core/project)
(require 'tool/spelling)
(require 'api/macro)

;; magit.
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

(defun tool/magit-status ()
  (interactive)
  (let ((default-directory (core/project-root)))
	(magit-status)))

(api/customize-set-variable*
 'magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ; magit uses the whole frame space
 'magit-diff-refine-hunk 'all)  ; word-wise diff highlight

;; git-link.
(defun tool/browse-project-home-page ()
  (interactive)
  (let ((git-link-open-in-browser t))
	(call-interactively 'git-link-homepage)))

;; git-commit.
(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
(add-hook 'git-commit-setup-hook #'(lambda () (display-line-numbers-mode -1)))

(provide 'tool/version-control)
