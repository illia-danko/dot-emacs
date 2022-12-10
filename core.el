;;; core.el --- Core Emacs configuration -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Illia Danko
;;
;; Author: Illia Danko <illia@danko.ws>
;; URL: https://github.com/illia-danko/dot-emacs

;; This file is not part of GNU Emacs.

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

(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1)) ; disable top menu buttons
(if (functionp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (functionp 'blink-cursor-mode)
    (blink-cursor-mode -1))
(if (functionp 'fringe-mode)
    (fringe-mode 0))

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p) ; enable y/n answers
(auto-fill-mode 1) ; intelligently breaks content into lines
(delete-selection-mode t) ;; delete the selection with a key-press
(show-paren-mode 1) ; highlight matched parentheses
(global-auto-revert-mode 1) ; keep fresh changes even if changed outside

(electric-pair-mode 1)
(save-place-mode 1) ; save cursor position when leave a buffer

;; UTF-8 only.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Dont ask for features.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)

(add-to-list 'completion-styles 'initials t) ;; completion fuzzy matching
;; $PATH.
;;; Find executables used by packages.
(let ((path `("/usr/local/go/bin"
              ,(concat (getenv "HOME") "/go/bin")
              ,(concat (getenv "HOME") "/.fzf/bin"))))

  ;; To make $PATH works correctly on Emacs GUI it's needed to set via both:
  ;; `exec-path' and `setenv'.
  (setq exec-path (append exec-path path))

  (mapc (lambda (p)
          (setenv "PATH" (concat (getenv "PATH") ":" p)))
        path))

;;; core.el ends here
