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

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      byte-compile-verbose nil
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 16777216
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                `(,(expand-file-name "lisp" user-emacs-directory))))

  (defvar my-shared-directory "~/.cache/emacs"
    "Cloud file storage location.")

  (setq custom-file (expand-file-name "settings.el" user-emacs-directory))
  (load custom-file)
  (let ((private-settings (expand-file-name "private-settings.el" my-shared-directory)))
    (and private-settings
         (file-exists-p private-settings)
         (load private-settings)))

  (load-file (expand-file-name "core.el" user-emacs-directory))
  (load-file (expand-file-name "internal.el" user-emacs-directory))
  (load-file (expand-file-name "packages.el" user-emacs-directory))
  (load-file (expand-file-name "abbrevs.el" user-emacs-directory)))
;; Load custom file (settings.el) before packages.
;; (load-file (expand-file-name "keymap-global.el" user-emacs-directory))

;; Compute and show Emacs warm time.
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
;;; init.el ends here
