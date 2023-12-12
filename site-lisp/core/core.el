(defvar core/emacs-config-directory "~/.config/emacs")

;; Disable tool bar.
(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))

;; Disable menu bar.
(unless (display-graphic-p)
  (with-eval-after-load 'menu-bar
    (menu-bar-mode -1)))

;; Disable scrool bar.
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))

;; Customize cursor.
(with-eval-after-load 'frame
  (customize-set-variable 'visible-cursor nil)
  (blink-cursor-mode -1))

;; Set bookmark's configuration file.
(with-eval-after-load 'bookmark
  (customize-set-variable 'bookmark-default-file (expand-file-name "bookmark" core/emacs-config-directory)))

;; Do not store backup files.
(with-eval-after-load 'files
  (customize-set-variable 'make-backup-files nil))

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

(provide 'core/core)
