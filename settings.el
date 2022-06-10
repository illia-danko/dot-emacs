;;; settings.el --- custom-file for variables and faces -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Elijah Danko
;;
;; Author: Elijah Danko <me@elijahdanko.net>
;; URL: https://github.com/elijahdanko/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Customizes Emacs variables and faces.

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

(defvar settings:shared-directory "~/.cache/emacs"
  "Cloud file storage location.")

(setq org-directory "~/github.com/elijahdanko/org")

(unless (file-directory-p settings:shared-directory)
  (make-directory settings:shared-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t)                       ; use abbreviations everywhere
 '(auto-revert-check-vc-info t)
 '(auto-revert-use-notify nil)
 '(auto-revert-verbose nil)             ; dont print `Reverting buffer' message
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(auto-save-list-file-prefix nil)
 '(backup-directory-alist `((".*" . ,temporary-file-directory)))
 '(c-basic-offset 4)
 '(comment-fill-column 80)
 '(company-posframe-quickhelp-show-header nil)
 '(company-posframe-show-indicator nil)
 '(company-posframe-show-metadata nil)
 '(company-posframe-show-params nil)
 '(custom-safe-themes t)        ; disable annoying warning while loading a theme
 '(dashboard-filter-agenda-entry 'dashboard-no-filter-agenda) ; show todo entries
 '(dashboard-items '((recents . 5) (projects . 5) (agenda . 10) (registers . 5)))
 '(dashboard-set-footer nil)
 '(dired-dwim-target t)
 '(dired-omit-files "^\\...+$") ; add hiden files to dired-omit-mode
 '(display-line-numbers-type 'relative)
 '(ediff-keep-variants nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(elfeed-search-filter "@6-months-ago +unread")
 '(elfeed-feeds
   '("http://nullprogram.com/feed/"
     ("https://planet.emacslife.com/atom.xml")
     ("https://blog.golang.org/feed.atom")
     ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL9KxKa8NpFxIcNQa9js7dQQIHc81b0-Xg" youtube emacs))) ; Mike Zamansky (Emacs)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-stay-out-of '(flymake) t) ; don't auto-build (execute flymake). Let flycheck doing so
 '(eldoc-echo-area-use-multiline-p nil)
 '(enable-local-variables :all) ; always trust .dir.locals.el (risk is accepted)
 '(enable-recursive-minibuffers t)
 '(fill-column 80)
 '(global-auto-revert-non-file-buffers t)
 '(gofmt-command "goimports")
 '(indent-tabs-mode nil)
 '(ivy-height 15)
 '(ivy-initial-inputs-alist nil)
 '(ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
 '(ivy-rich-path-style 'abbrev) ; shorten style path, eg: /home/user1/Documents -> ~/Documents
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 4)
 '(kill-whole-line t)
 '(magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
 '(markdown-fontify-code-blocks-natively t) ; highlight code block syntax
 '(markdown-hide-markup t)                  ; hide urls
 '(projectile-completion-system 'ivy)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)          ; turn off ring bell sound
 '(set-mark-command-repeat-pop t) ; pop up mark in transitive way: don't repeat C-u C-SPC
 '(tab-width 4)
 '(use-dialog-box nil)
 '(user-full-name "Elijah Danko")
 '(user-mail-address "me@elijahdanko.net")
 '(vc-follow-symlinks t)             ; turn off confirmation when open a symlink
 '(visible-cursor nil)               ; dont blink cursor in tty.
 `(bookmark-default-file ,(expand-file-name "bookmarks" settings:shared-directory)) ; bookmarks path
 `(code-review-db-path ,(expand-file-name "code-review-db-file.sqlite" settings:shared-directory))
 `(forge-database-file ,(expand-file-name "forge-database.sqlite" settings:shared-directory))
 `(projectile-known-projects-file ,(expand-file-name "projectile-bookmarks.eld" settings:shared-directory)) ; saved projects path
 `(recentf-save-file ,(expand-file-name "recentf" settings:shared-directory)) ; recent files path
 `(save-place-file ,(expand-file-name "places" settings:shared-directory)) ; saved file positions file path
 `(smex-save-file ,(expand-file-name "smex-items" settings:shared-directory))
 `(transient-history-file ,(expand-file-name "transient/history.el" settings:shared-directory))
 `(transient-levels-file ,(expand-file-name "transient/levels.el" settings:shared-directory)) ; transient-mode cache files
 `(transient-values-file ,(expand-file-name "transient/values.el" settings:shared-directory))
 `(url-configuration-directory ,(expand-file-name "url" settings:shared-directory))
 )

(custom-set-variables
 '(org-agenda-block-separator "")
 '(org-agenda-files (list org-default-notes-file))
 '(org-capture-templates `(("n" "[n]ew TODO item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")))
 '(org-ellipsis "  " ) ; folding symbol
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-emphasis-markers t) ; close links, etc.
 '(org-pretty-entities t) ; show LaTeX-like symbols as UTF-8 characters
 '(org-startup-folded t)
 `(org-default-notes-file ,(expand-file-name "todo.org" org-directory)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :height 110))))
 '(fixed-pitch ((t (:family "Iosevka Term" :height 110))))
 '(variable-pitch ((t (:family "Iosevka Term" :height 110)))))

;;; settings.el ends here
