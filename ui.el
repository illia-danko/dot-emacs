;;; ui.el --- Emacs User Interface -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Elijah Danko
;;
;; Author: Elijah Danko < me@elijahdanko.net>
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

(use-package doom-themes
  :straight t)

(defvar theme:file-path "~/.emacs.d/theme"
  "Emacs theme filepath.")

(defvar theme:default-name "doom-dark+"
  "Current Emacs theme.")

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun theme:save-to-file (path name)
  (with-temp-buffer
    (insert name)
    (write-region (point-min) (point-max) path)))

(defun theme:save-current-to-flie ()
  (theme:save-to-file theme:file-path
                      (symbol-name (car custom-enabled-themes))))

(defun theme:ensure-exists ()
  (unless (file-exists-p theme:file-path)
    (theme:save-to-file theme:file-path theme:default-name)))

(defun theme:load-from-file ()
  (theme:ensure-exists)
  (load-theme
   (intern
    (string-trim
     (with-temp-buffer
       (insert-file-contents theme:file-path)
       (buffer-string))))
   t))

(add-hook 'after-load-theme-hook #'theme:save-current-to-flie)

(defun theme:update-faces (&optional frame)
  "Adjust faces on theme loading.
Use a default vertical border face."
  (unless (display-graphic-p)
    (when frame
      (select-frame frame))
    (set-face-attribute 'vertical-border frame
                        :foreground (face-foreground 'success)
                        :background (face-background 'default))))

(add-hook 'after-load-theme-hook #'theme:update-faces)

(use-package custom
  :config
  (theme:load-from-file))

(use-package which-key
  :straight t
  :config (which-key-mode 1))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

;; Maximize window on startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package window
  :init
  (defun split-window:jump-right ()
    "Acts as `split-window-right' but also preforms jump to the window."
    (interactive)
    (split-window-right)
    (other-window 1))
  :bind (("C-x 3" . split-window:jump-right)))

;; Highlight RGB(A) and color names.
(use-package rainbow-mode
  :straight t
  :bind (("C-c t c" . rainbow-mode)))

;;; ui.el ends here
