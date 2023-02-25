;; Generic configuration.
(use-package emacs
  :hook
  ((prog-mode org-mode markdown-mode yaml-mode) . (lambda ()
													(setq-local show-trailing-whitespace t)))
  :custom
  (tab-width 4)  ; number spaces per a tab
  (ring-bell-function 'ignore) ; stop ring bell alarms
  (fill-column 100) ; nowadays 100 characters wide sounds reasonable on large screens
  (set-mark-command-repeat-pop t) ; don't repeat C-u previx on mark commands (i.e. C-u C-SPC)
  :config
  (fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
  (put 'upcase-region 'disabled nil) ; don't confirm on upcase command
  (put 'downcase-region 'disabled nil) ; don't confirm on downcase command
  (column-number-mode) ; show column number on modeline
  )

;; Line-numbers on the fringe side.
(use-package display-line-numbers
  :hook ((prog-mode conf-mode yaml-mode markdown-mode org-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type t))

;; Disable tool bar.
(use-package tool-bar
  :config
  (tool-bar-mode -1))

;; Enable menu-bar on MACOS graphic only.
(use-package menu-bar
  :unless (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (menu-bar-mode -1))

;; Disable scroll bar.
(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

;; Stop the cursor blinking.
(use-package frame
  :custom
  (visible-cursor nil) ; for terminal
  :config
  (blink-cursor-mode -1) ; for gui
  )

;; Use command as meta and use ctrl as alt on MACOS.
(use-package ns-win
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'control))

;; Command hints.
(use-package which-key :straight t
  :config
  (which-key-mode 1))

;; Do not store backup files.
(use-package files
  :custom
  (make-backup-files nil))

;; Override selection on yank.
(use-package delsel
  :config (delete-selection-mode))

;; Highlight TODO,BUG,FIXME,NOTE,etc. comment keywords.
(use-package hl-todo :straight t
  :config (global-hl-todo-mode))

;; Store and restore last edit position of a file.
(use-package saveplace
  :config
  (save-place-mode 1))

;; Store recent edit file names.
(use-package recentf
  :config
  (recentf-mode 1))

;; Keep yank history after exiting Emacs.
(use-package undohist :straight t
  :custom
  (undohist-ignored-files '("COMMIT_EDITMSG") ; disable warning on temp files
  :config
  (undohist-initialize)))

;; Check spelling using `hunspell'.
(use-package flyspell
  :init
  (defun my-flyspell-toggle ()
    (interactive)
    (if (symbol-value flyspell-mode)
		(progn
		  (message "Flyspell off")
		  (flyspell-mode -1))
	  (progn
		(message "Flyspell on")
		(if (derived-mode-p 'prog-mode)
			(flyspell-prog-mode)
		  (flyspell-mode))
		(flyspell-buffer))))

  :hook
  (git-commit-setup . flyspell-mode) ; check COMMIT_EDITMSG buffer for spelling

  :bind
  ("C-c ts" . my-flyspell-toggle))

;; Echo area documentation hints.
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil) ; do not enlarge echo area.
  )

;; Emacs startup greeter.
(use-package dashboard :straight t
  :init
  ;; Open dashboard when frame created.
  (add-hook 'after-make-frame-functions
            (lambda (&optional frame)
              (setq initial-buffer-choice (lambda nil
                                            (get-buffer "*dashboard*")))))

  :custom
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda) ; show todo entries
  (dashboard-items '((agenda . 8) (projects . 4) (recents . 4))) ; layout
  (dashboard-projects-backend 'project-el) ; use project-el as project backend

  :config
  (dashboard-setup-startup-hook))

(provide 'init-core)
