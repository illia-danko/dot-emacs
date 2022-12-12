;;; settings.el --- custom-file for variables and faces -*- lexical-binding: t -*-
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

(defvar u:shared-directory "~/.cache/emacs"
  "Cloud file storage location.")

(defvar u:prettier-config-path (concat
                                "--config="
                                (expand-file-name "~/.config/prettier/prettier.config.js"))
  "Path to prettier config.")

(setq org-directory "~/github.com/illia-danko/org")

(unless (file-directory-p u:shared-directory)
  (make-directory u:shared-directory))

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
 '(custom-safe-themes t)        ; disable annoying warning while loading a theme
 '(dashboard-filter-agenda-entry 'dashboard-no-filter-agenda) ; show todo entries
 '(dashboard-items '((agenda . 8) (projects . 4) (recents . 4)))
 '(dashboard-set-footer nil)
 '(dired-dwim-target t)
 '(dired-omit-files "^\\...+$")         ; add hiden files to dired-omit-mode
 '(dired-omit-verbose nil)
 '(display-line-numbers-type t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-stay-out-of '(flymake) t) ; don't auto-build (execute flymake). Let flycheck doing so
 '(eldoc-echo-area-use-multiline-p nil)
 '(elfeed-search-filter "@6-months-ago +unread")
 '(enable-local-variables :all) ; always trust .dir.locals.el (risk is accepted)
 '(enable-recursive-minibuffers t)
 '(fill-column 80)
 '(format-all-default-formatters `(("YAML" (prettier ,u:prettier-config-path)) ("JavaScript" (prettier ,u:prettier-config-path))))
 '(global-auto-revert-non-file-buffers t)
 '(gofmt-command "goimports")
 '(indent-tabs-mode nil)
 '(inhibit-splash-screen t)
 '(js-indent-level 4)
 '(kill-whole-line t)
 '(mac-command-modifier 'meta)
 '(mac-option-modifier 'control)
 '(magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
 '(markdown-fontify-code-blocks-natively t) ; highlight code block syntax
 '(markdown-hide-markup t)                  ; hide urls
 '(prefix-help-command #'embark-prefix-help-command)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)          ; turn off ring bell sound
 '(set-mark-command-repeat-pop t) ; pop up mark in transitive way: don't repeat C-u C-SPC
 '(tab-width 4)
 '(use-dialog-box nil)
 '(user-full-name "Illia Danko")
 '(user-mail-address "illia@danko.ws")
 '(vc-follow-symlinks t)             ; turn off confirmation when open a symlink
 '(visible-cursor nil)               ; dont blink cursor in tty.
 '(xref-show-definitions-function 'consult-xref)
 '(xref-show-xrefs-function 'consult-xref)
 `(bookmark-default-file ,(expand-file-name "bookmarks" u:shared-directory)) ; bookmarks path
 `(projectile-known-projects-file ,(expand-file-name "projectile-bookmarks.eld" u:shared-directory)) ; saved projects path
 `(recentf-save-file ,(expand-file-name "recentf" u:shared-directory)) ; recent files path
 `(save-place-file ,(expand-file-name "places" u:shared-directory)) ; saved file positions file path
 `(transient-history-file ,(expand-file-name "transient/history.el" u:shared-directory))
 `(transient-levels-file ,(expand-file-name "transient/levels.el" u:shared-directory)) ; transient-mode cache files
 `(transient-values-file ,(expand-file-name "transient/values.el" u:shared-directory))
 `(url-configuration-directory ,(expand-file-name "url" u:shared-directory))
 )

(custom-set-variables
 '(org-agenda-block-separator "")
 '(org-agenda-files (list org-default-notes-file))
 '(org-capture-bookmark nil) ; prevent storing bookmarks
 '(org-capture-templates `(("n" "[n]ew TODO item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")))
 '(org-ellipsis "  " ) ; folding symbol
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-emphasis-markers t) ; close links, etc.
 '(org-pretty-entities t) ; show LaTeX-like symbols as UTF-8 characters
 '(org-startup-indented t)
 '(org-superstar-leading-bullet ?\s)
 `(org-default-notes-file ,(expand-file-name "todo.org" org-directory)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Nerd Font Mono" :height 160))))
 '(fixed-pitch ((t (:family "Iosevka Nerd Font Mono" :height 160))))
 '(variable-pitch ((t (:family "Iosevka Nerd Font Mono" :height 160)))))

;;; settings.el ends here
