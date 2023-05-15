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

(provide 'init-project)
