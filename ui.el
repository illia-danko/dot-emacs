;;; ui.el --- Emacs User Interface -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Illia A. Danko
;;
;; Author: Illia A. Danko <illia@idanko.net>
;; URL: https://github.com/idanko/emacs.d

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
(require 'diminish)

(use-package which-key
  :straight t
  :diminish
  :config (which-key-mode 1))

(use-package leuven-theme
  :straight t
  :config
  (load-theme 'leuven t))

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
(use-package olivetti
  :straight t
  :bind (("C-c t SPC" . olivetti-mode)))

;; Highlight RGB(A) and color names.
(use-package rainbow-mode
  :straight t
  :bind (("C-c t c" . rainbow-mode)))
