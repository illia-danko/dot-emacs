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
  ("C-c gg" . magit-status)
  ("C-c gd" . magit-diff-buffer-file)
  ("C-c gl" . magit-log-all)
  ("C-c gL" . magit-log-buffer-file)
  ("C-c gb" . magit-blame-addition))

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
Prohibit command prompt on `project-switch-project', instead directly execute `project-find-file'."
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

(use-package git-gutter :straight t
  :init
  (defun my-git-gutter-refresh-hunks (&rest _)
    (interactive)
    (git-gutter:update-all-windows))

  (defun my-git-gutter-enable (&rest args)
    (interactive)
    (git-gutter-mode 1))

  (defun my-git-gutter-popup-hunk-jump (&optional diffinfo)
    (interactive)
    (git-gutter:popup-hunk diffinfo)
    (switch-to-buffer-other-window git-gutter:popup-buffer))

  (global-git-gutter-mode 1)

  :hook ((after-change-major-mode . my-git-gutter-enable))

  :custom
  (left-margin-width 1) ; add space for git-gutter
  (git-gutter:added-sign "")
  (git-gutter:ask-p nil)
  (git-gutter:deleted-sign "")
  (git-gutter:modified-sign "")

  :config
  (advice-add 'kill-buffer :after #'my-git-gutter-refresh-hunks)

  :custom-face
  (git-gutter:added ((t (:inherit 'modus-themes-prompt :background "defualt"))))
  (git-gutter:deleted  ((t (:inherit 'error :background "default"))))
  (git-gutter:modified ((t (:inherit 'modus-themes-heading-3 :background "default"))))

  :bind
  ("C-c n" . git-gutter:next-hunk)
  ("C-c p" . git-gutter:next-hunk)
  ("C-c hu" . git-gutter:revert-hunk)
  ("C-c hp" . my-git-gutter-popup-hunk-jump)
  )

(provide 'init-project)
