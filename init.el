;;; init.el --- Illia Danko (illia@danko.ws) Emacs dotfiles -*- lexical-binding: t -*-
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

(defvar time-emacs-start (current-time)
  "Time Emacs has started.")

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      load-prefer-newer t
      font-lock-verbose nil
      byte-compile-verbose nil)

(add-hook 'emacs-startup-hook
	      (lambda ()
	        ;; After startup, it is important you reset this to some
            ;; reasonable default. A large gc-cons-threshold will
            ;; cause freezing and stuttering during long-term
            ;; interactive use."
            (setq gc-cons-threshold 16777216
		          gc-cons-percentage 0.1)))

;; Load custom file (settings.el) before packages.
(setq custom-file (expand-file-name "settings.el" user-emacs-directory))
(load custom-file)
(let ((private-settings.el (expand-file-name "private-settings.el" intern:shared-directory)))
  (and private-settings.el
       (file-exists-p private-settings.el)
       (load private-settings.el)))

(load-file (expand-file-name "core.el" user-emacs-directory))
(load-file (expand-file-name "internal.el" user-emacs-directory))
(load-file (expand-file-name "packages.el" user-emacs-directory))
(load-file (expand-file-name "abbrevs.el" user-emacs-directory))
(load-file (expand-file-name "keymap-global.el" user-emacs-directory))

;; Compute and show Emacs warm time.
(message "Load time %.06f" (float-time (time-since time-emacs-start)))

;;; init.el ends here
