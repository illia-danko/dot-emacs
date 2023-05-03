;; Always open symlinks without confirmation.
(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

;; Workhorse git client.
(use-package magit :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1) ; magit uses the whole frame space
  (magit-diff-refine-hunk 'all)  ; word-wise diff highlight

  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-diff-buffer-file)
  ("C-c L" . magit-log-all)
  ("C-c l" . magit-log-buffer-file))

;; Copy/open git urls.
(use-package git-link :straight t
  :init
  (defun my-git-link-open-page ()
    (interactive)
    (let ((git-link-open-in-browser t))
      (call-interactively 'git-link-homepage)))

  :bind (("C-c u" . git-link)
         ("C-c U" . #'my-git-link-open-page)))

;; Emacs diff tool.
(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally) ; split buffers horizontally
  )

;; Navigate and manipulate projects on the machine.
(use-package project
  :config
  (defun project-switch-project (dir)
	"Override default `project-switch-project' command.
Instead of prompt the list with commands, directly execute `project-find-file'.
Behave as `projectile-switch-project'."
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (call-interactively 'project-find-file)))

  :bind
  ("C-x f" . project-find-file)
  ("C-x p" . project-switch-project)
  ("C-x #" . project-kill-buffers)
  ([remap server-edit] . project-kill-buffers))

(provide 'init-vc)
