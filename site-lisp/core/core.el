(require 'api/macro)

;; Core.
(require 'emacs)
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
 'inhibit-startup-message t ; do not show Emacs startup log message
 'create-lockfiles nil ; privent creation files with hashes, e.g. #file#
 'backup-directory-alist `((".*" . ,temporary-file-directory))
 'auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 )

(fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
(put 'upcase-region 'disabled nil) ; don't confirm on upcase command
(put 'downcase-region 'disabled nil) ; don't confirm on downcase command
(column-number-mode 1) ; show column number on modeline

(defun core/toggle-display-whitespaces ()
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
(require 'tool-bar)
(tool-bar-mode -1)

;; Disable menu bar.
(require 'menu-bar)
(menu-bar-mode -1)

;; Disable scrool bar.
(require 'scroll-bar)
(scroll-bar-mode -1)

;; Customize cursor.
(require 'frame)
(api/customize-set-variable* 'visible-cursor nil)
(blink-cursor-mode -1)

;; Set bookmark's configuration file.
(require 'bookmark)
(api/customize-set-variable*
 'bookmark-default-file (expand-file-name "bookmarks" core/emacs-config-directory))

;; Do not store backup files.
(require 'files)
(api/customize-set-variable* 'make-backup-files nil)

;; Delete selection on yank (override selected text).
(require 'delsel)
(delete-selection-mode 1)

;; Restore last edit position of a file.
(require 'saveplace)
(save-place-mode 1)

;; Keep track of recent open files.
(require 'recentf)
(recentf-mode 1)

;; Keep track of minibuffer history.
(require 'savehist)
(savehist-mode 1)

(require 'display-line-numbers)
(api/customize-set-variable*
 'display-line-numbers-type 'relative)

(provide 'core/core)
