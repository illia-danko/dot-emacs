;; Generic configuration.
(use-package emacs
  :init
  (defun my-backward-kill-word-or-region (&optional arg)
	"If mark is active acts as `C-w' otherwise as `backward-kill-word'."
	(interactive "p")
	(if mark-active
		(kill-region (mark) (point))
      (backward-kill-word arg)))

  (defun my-smart-tab ()
    (interactive)
    (call-interactively
     (cond ((<= (current-column)(current-indentation))
            #'indent-for-tab-command)
           ((and (bound-and-true-p yas-minor-mode)
                 (yas--templates-for-key-at-point))
            #'yas-expand)
           ((and (bound-and-true-p yas-minor-mode)
                 (bound-and-true-p emmet-mode))
            #'emmet-expand-yas)
           ((bound-and-true-p emmet-mode)
            #'emmet-expand-line)
           (t #'indent-for-tab-command))))
  (call-interactively #'my-smart-tab)

  :hook
  ((prog-mode org-mode markdown-mode yaml-mode) . (lambda ()
													(setq-local show-trailing-whitespace t)))
  :custom
  (tab-width 4)  ; number spaces per a tab
  (ring-bell-function 'ignore) ; stop ring bell alarms
  (fill-column 80) ; 80 characters per a line
  (comment-fill-column 80)
  (set-mark-command-repeat-pop t) ; don't repeat C-u prefix on mark commands (i.e. C-u C-SPC)
  (warning-minimum-level :error) ; don't show warnings
  (truncate-lines t)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)

  :config
  (fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
  (put 'upcase-region 'disabled nil) ; don't confirm on upcase command
  (put 'downcase-region 'disabled nil) ; don't confirm on downcase command
  (column-number-mode) ; show column number on modeline
  (global-set-key (kbd "C-z") nil) ; do not suspend-frame

  :bind
  ("C-w" . my-backward-kill-word-or-region)
  ("TAB" . my-smart-tab)
  ([remap kill-buffer] . kill-this-buffer))

;; Highlight on the cursor line.
(use-package hl-line-mode
  :hook
  ((prog-mode text-mode dashboard-mode restclient-mode) . hl-line-mode))

;; Highlight parentheses different colors.
(use-package rainbow-delimiters :straight t
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . rainbow-delimiters-mode))

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
  ;; (mac-option-modifier 'control) ;; Karabiner-elments already remap opt to ctrl
  )

(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" my-emacs-config-directory)))

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
  :bind
  ("C-c r" . consult-recent-file)
  :config
  (recentf-mode 1))

;; Setup fringe. Used by some modes like `git-gutter-fringe'.
(use-package fringe
  :custom
  (left-fringe-width 8)
  (right-fringe-width 8)

  :custom-face
  (fringe ((t (:inherit 'default :background "defualt"))))

  :config
  (fringe-mode 1))

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
  ("C-c l" . my-flyspell-toggle))

;; Echo area documentation hints.
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil) ; do not enlarge echo area.
  )

;; Open URL at point in the browser. The `C-c C-o' shortcut is compatible with
;; `markdown-mode' and `org-mode'.
(use-package browse-url
  :bind
  ("C-c C-o" . browse-url-at-point))

;; Real temrinal emulator.
(use-package vterm :straight t
  :init
  (defun my-project-vterm (&optional args)
    (interactive)
    (let ((default-directory (or (ignore-errors
                                   (project-root (project-current)))
                                 default-directory)))
      (vterm)))

  :bind
  ("C-c t" . my-project-vterm)
  ("C-c T" . vterm))

(use-package eshell
  :bind
  ("C-c E" . eshell)
  ("C-c e" . project-eshell))

;; Line-numbers on the fringe side.
(use-package display-line-numbers
  :custom-face
  (line-number ((t (:inherit 'default :background "defualt"))))

  :bind
  ("C-c q" . display-line-numbers-mode))

;; Code editing rules per a project.
(use-package editorconfig :straight t
  :hook
  (prog-mode . editorconfig-mode))

(provide 'init-core)
