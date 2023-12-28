(require 'api/variable)

(with-eval-after-load 'emacs
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
			   (if trailing-whitespace-p "enabled" "disabled")))))

(defvar core/emacs-config-directory "~/.config/emacs")

;; Load private-settings.el.
(let ((private-settings (expand-file-name "private-settings.el" core/emacs-config-directory)))
  (and private-settings
       (file-exists-p private-settings)
       (load private-settings)))

;; Disable tool bar.
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))

;; Disable menu bar.
(with-eval-after-load 'menu-bar
  (menu-bar-mode -1))

;; Disable scrool bar.
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))

;; Customize cursor.
(with-eval-after-load 'frame
  (api/customize-set-variable* 'visible-cursor nil)
  (blink-cursor-mode -1))

;; Set bookmark's configuration file.
(with-eval-after-load 'bookmark
  (api/customize-set-variable*
   'bookmark-default-file (expand-file-name "bookmarks" core/emacs-config-directory)))

;; Do not store backup files.
(with-eval-after-load 'files
  (api/customize-set-variable* 'make-backup-files nil))

;; Delete selection on yank (override selected text).
(progn
  (with-eval-after-load 'delsel
	(delete-selection-mode 1))

  (require 'delsel))

;; Restore last edit position of a file.
(progn
  (with-eval-after-load 'saveplace
	(save-place-mode 1))

  (require 'saveplace))

(progn
  (with-eval-after-load 'recentf
	(recentf-mode 1))

  (require 'recentf))

;; Store minibuffer history.
(progn
  (with-eval-after-load 'savehist
	(savehist-mode 1))

  (require 'savehist))

(progn
  (with-eval-after-load 'eldoc
	(api/customize-set-variable* 'eldoc-echo-area-use-multiline-p nil)))

;; Eager loading.
(require 'imenu)
(require 'recentf)

(provide 'core/core)
