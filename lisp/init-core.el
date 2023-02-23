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
  (put 'downcase-region 'disabled nil) ; con't confirm on downcase
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
  :config
  (blink-cursor-mode -1))

;; Use command as meta and use ctrl as alt on MACOS.
(use-package ns-win
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'control))

;; Strart Emacs maximized, use custom font.
(use-package frame
  :init
  (defun my-adjust-created-frame ()
    (set-frame-font
     "-*-Iosevka Nerd Font Mono-semibold-normal-normal-*-16-*-*-*-m-0-iso10646-1" nil t)
    (toggle-frame-maximized))
  :hook
  (window-setup . my-adjust-created-frame))

;; Command hints.
(use-package which-key :straight t
  :config
  (which-key-mode 1))

;; Do not store backup files.
(use-package files
  :custom
  (make-backup-files nil))

;; Copy to clipboard on teminal.
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

(provide 'init-core)
