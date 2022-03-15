;;; ui.el --- Emacs User Interface -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Elijah Danko
;;
;; Author: Elijah Danko < me@eli.net>
;; URL: https://github.com/elijahdanko/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains theme settings, modeline and font configuration.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'use-package)

(use-package which-key
  :straight t
  :config (which-key-mode 1))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t))

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode +1))

;; Maximize window on startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

(add-hook 'after-make-frame-functions
	  (lambda (&optional frame)
	    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))

(use-package window
  :init
  (defun split-window:jump-right ()
    "Acts as `split-window-right' but also preforms jump to the window."
    (interactive)
    (split-window-right)
    (other-window 1))
  :bind (("C-x 3" . split-window:jump-right)))

 ;; Move buffer to the middle of the screen.
(use-package olivetti :straight t)

(use-package hide-mode-line
  :straight t
  :init
  (defun distraction-free-toggle (&optional arg)
    (interactive)
    (call-interactively 'olivetti-mode arg)
    (call-interactively 'hide-mode-line-mode arg))
  :after (olivetti)
  :bind (("C-c SPC SPC" . distraction-free-toggle)))

;; Highlight RGB(A) and color names.
(use-package rainbow-mode
  :straight t
  :bind (("C-c t c" . rainbow-mode)))

;; Hooks.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun update-faces:hook (&optional frame)
  (unless (display-graphic-p)
    (when frame
      (select-frame frame))
    ;; Fix terminal vertical-border glyph.
    ;; (https://emacs.stackexchange.com/questions/7228/nice-tty-window-borders-in-24-4).
    (let ((display-table (or standard-display-table (make-display-table))))
      (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
      (setq standard-display-table display-table))
    ;; Make vertical border as tmux' one.
    (set-face-attribute 'vertical-border frame
                        :foreground (face-foreground 'success)
                        :background (face-background 'default))))

(add-hook 'after-load-theme-hook #'update-faces:hook)
(add-hook 'after-make-frame-functions #'update-faces:hook)
