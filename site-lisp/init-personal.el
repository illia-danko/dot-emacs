(use-package emacs
  :init
  (defun my-backward-kill-word-or-region (&optional arg)
	"If mark is active acts as `C-w' otherwise as `backward-kill-word'."
	(interactive "p")
	(if mark-active
		(kill-region (mark) (point))
      (backward-kill-word arg)))

  (defun my-smart-tab (&optional arg)
    (interactive "p")
    (call-interactively
     (cond ((<= (current-column) (current-indentation))
            #'indent-for-tab-command)
		   ((and (fboundp 'tempel-expand)
				 (tempel--prefix-bounds))
            #'tempel-expand)
           (t #'indent-for-tab-command))))
  (call-interactively #'my-smart-tab)

  :bind
  ("C-w" . my-backward-kill-word-or-region)
  ("TAB" . my-smart-tab)
  ([remap kill-buffer] . kill-this-buffer))

(use-package project
  :config
  (defun project-switch-project (dir)
	"Override default `project-switch-project' command.
Instead of prompt the list with commands, directly execute `project-find-file'.
Behave as `projectile-switch-project'."
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (call-interactively 'project-find-file))))

(provide 'init-personal)
