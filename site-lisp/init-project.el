;; Always open symlinks without confirmation.

;; Emacs diff tool.
(use-package ediff
  :init
  (defun my-ediff-compare-two-open-windows ()
    (interactive)
    (let* ((windows (window-list))
           (buffer-a (window-buffer (nth 1 windows)))
           (buffer-b (window-buffer (nth 2 windows))))
      (ediff-buffers buffer-a buffer-b)))

  :bind
  ("C-c oe" . my-ediff-compare-two-open-windows)

  :custom
  (ediff-split-window-function 'split-window-horizontally) ; split buffers horizontally
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; use one frame for diff
  )

(use-package diff
  :bind
  ("C-c gf" . diff-buffer-with-file))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package vc
  :bind
  ("C-c ge" . vc-ediff))

(defun my-project-root ()
  (or (ignore-errors (project-root (project-current)))
      default-directory))

;; Workhorse git client.
(use-package magit :straight t
  :init
  (defun my-push-org-to-current-repository ()
    "Stage, commit and push to upstream a personal org note file."
    (interactive)
    (let* ((fullname (buffer-file-name))
           (relname (file-name-nondirectory fullname))
           (current-project (my-project-root)))
      (if (string-prefix-p (expand-file-name org-directory) fullname)
          (progn
            (call-process "git" nil nil nil "add" fullname)
            (call-process "git" nil nil nil "commit" "-m" (format "Update %s" relname))
            (call-process "git" nil nil nil "push")
            (message "Pushed %s" relname))
        (message "%S not a personal org note file" fullname)
        )))

  (defun my-push-org-to-current-repository-1 ()
    (interactive)
    (my-push-org-to-current-repository)
    (and (boundp git-gutter-fringe)
         (boundp git-gutter-mode)
         (git-gutter:update-all-windows)))

  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1) ; magit uses the whole frame space
  (magit-diff-refine-hunk 'all)  ; word-wise diff highlight

  :bind
  ("C-x g"  . magit-status)
  ("C-c gg" . magit-status)
  ("C-c gd" . magit-diff-buffer-file)
  ("C-c gl" . magit-log-buffer-file)
  ("C-c gL" . magit-log-all)
  ("C-c gb" . magit-blame-addition)
  ("C-c gc" . my-push-org-to-current-repository-1)
  ("C-c gr" . magit-diff-range))

;; Copy/open git urls.
(use-package git-link :straight t
  :init
  (defun my-git-link-open-page ()
    (interactive)
    (let ((git-link-open-in-browser t))
      (call-interactively 'git-link-homepage)))

  :bind (("C-c gu" . git-link)
         ("C-c gU" . #'my-git-link-open-page)))

;; Navigate and manipulate projects on the machine.
(use-package project
  :config
  (defun project-switch-project (dir)
	"Override default `project-switch-project' command.
Prohibit command prompt on `project-switch-project', instead directly execute `project-find-file'."
    (interactive (list (project-prompt-project-dir)))
    (let ((project-current-directory-override dir))
      (call-interactively 'project-find-file)))

  :bind
  ("C-x f" . project-find-file)
  ("C-x p" . project-switch-project)
  ("C-x !" . project-forget-zombie-projects)
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
                          `                          (delete-windows-on buf)))
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

(use-package git-gutter-fringe :straight t
  :init
  (defun my-git-gutter-popup-hunk-jump (&optional diffinfo)
    (interactive)
    (git-gutter:popup-hunk diffinfo)
    (switch-to-buffer-other-window git-gutter:popup-buffer))

  (require 'git-gutter-fringe)

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

  (global-git-gutter-mode 1)

  :custom
  (git-gutter:ask-p nil)
  (git-gutter-fr:side 'right-fringe)

  :custom-face
  (git-gutter-fr:added ((t (:inherit 'modus-themes-prompt :background "defualt"))))
  (git-gutter-fr:deleted  ((t (:inherit 'error :background "default"))))
  (git-gutter-fr:modified ((t (:inherit 'modus-themes-heading-3 :background "default"))))

  :bind
  ("C-c n" . git-gutter:next-hunk)
  ("C-c p" . git-gutter:previous-hunk)
  ("C-c hu" . git-gutter:revert-hunk)
  ("C-c hp" . my-git-gutter-popup-hunk-jump)
  )

(provide 'init-project)
