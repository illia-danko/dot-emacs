(require 'emacs)
(require 'tool-bar)
(require 'menu-bar)
(require 'scroll-bar)
(require 'frame)
(require 'bookmark)
(require 'files)
(require 'delsel)
(require 'saveplace)
(require 'recentf)
(require 'savehist)
(require 'eldoc)
(require 'imenu)
(require 'api/macro)

;; Core.
(api/customize-set-variable*
 'tab-width 4  ; number spaces per a tab
 'ring-bell-function 'ignore ; stop ring bell alarms
 'fill-column 100 ; 100 characters per a line
 'comment-fill-column 100
 'set-mark-command-repeat-pop t ; do not repeat C-u prefix on mark commands (i.e. C-u C-SPC)
 'warning-minimum-level :error ; do not show warnings
 'truncate-lines t ; do not wrap long lines
 'tab-always-indent 'complete
 'enable-local-variables :all ; run .dir-locals.el with no dialog
 'mac-command-modifier 'meta ; use command key as meta
 ;; Karabiner-elments already remap opt to ctrl
 ;; (cusotomize-set-variable 'mac-option-modifier 'control)
 'vc-follow-symlinks t ; always follow a symlink when accessing a file
 'auth-source-save-behavior nil ; do not ask to save passwords file using tramp
 )

(fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
(put 'upcase-region 'disabled nil) ; don't confirm on upcase command
(put 'downcase-region 'disabled nil) ; don't confirm on downcase command
(column-number-mode 1) ; show column number on modeline

(defun core/toggle-highlight-whitespaces ()
  (interactive)
  (let ((trailing-whitespace-p (not show-trailing-whitespace)))
	(setq-local show-trailing-whitespace
				trailing-whitespace-p)
	(message "Trailing whitespace mode %s"
			 (if trailing-whitespace-p "enabled" "disabled"))))

(defvar core/emacs-config-directory "~/.config/emacs")

;; Load private-settings.el.
(let ((private-settings (expand-file-name "private-settings.el" core/emacs-config-directory)))
  (and private-settings
       (file-exists-p private-settings)
       (load private-settings)))

;; Disable tool bar.
(tool-bar-mode -1)

;; Disable menu bar.
(menu-bar-mode -1)

;; Disable scrool bar.
(scroll-bar-mode -1)

;; Customize cursor.
(api/customize-set-variable* 'visible-cursor nil)
(blink-cursor-mode -1)

;; Set bookmark's configuration file.
(api/customize-set-variable*
 'bookmark-default-file (expand-file-name "bookmarks" core/emacs-config-directory))

;; Do not store backup files.
(api/customize-set-variable* 'make-backup-files nil)

;; Delete selection on yank (override selected text).
(delete-selection-mode 1)

;; Restore last edit position of a file.
(save-place-mode 1)

;; Keep track of recent open files.
(recentf-mode 1)

;; Keep track of minibuffer history.
(savehist-mode 1)

;; Disable echo area documentation.
(global-eldoc-mode -1)


(provide 'core/core)
