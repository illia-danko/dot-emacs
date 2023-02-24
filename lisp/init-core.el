;; Generic configuration.
(use-package emacs
  :hook
  ((prog-mode org-mode markdown-mode yaml-mode) . (lambda ()
													(setq-local show-trailing-whitespace t)))
  :custom
  (tab-width 4)  ; number spaces per a tab
  (ring-bell-function 'ignore) ; stop ring bell alarms
  (fill-column 100) ; nowadays 100 characters wide sounds reasonable on large screens
  :config
  (fset 'yes-or-no-p 'y-or-n-p) ; type less on yes/no questions
  (put 'upcase-region 'disabled nil) ; don't confirm on upcase
  (put 'downcase-region 'disabled nil) ; don't confirm on downcase
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

;; Copy to clipboard on terminal.
(use-package xclip :straight t
  :unless (display-graphic-p)
  :config
  (xclip-mode 1))

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

;; Store last edit file names.
(use-package recentf
  :config
  (recentf-mode 1))

;; Keep yank history after closing emacs.
(use-package undohist :straight t
  :custom
  (undohist-ignored-files '("COMMIT_EDITMSG") ; disable warning of temp files
  :config
  (undohist-initialize)))

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
  (git-commit-setup . flyspell-mode)

  :bind
  ("C-c ts" . my-flyspell-toggle))

;; Center the screen (part of distructon free).
(use-package olivetti :straight t)

;; Hide modeline (part of distructon free).
(use-package hide-mode-line :straight t)

;; Tmux integration.
(use-package ttymux
  :straight '(ttymux
              :type git
              :host github
              :repo "illia-danko/ttymux.el")
  :unless (display-graphic-p)
  :config
  (ttymux-mode 1))

;; Echo area documentation hints.
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil) ; do not enlarge echo area.
  )

(provide 'init-core)
