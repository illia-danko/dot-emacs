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

(defvar my:config-path-prettier (concat
                               "--config="
                               (expand-file-name "~/.config/prettier/prettier.config.js"))
  "Path to prettier config.")

(setq org-directory "~/github.com/illia-danko/org")

(unless (file-directory-p settings:shared-directory)
  (make-directory settings:shared-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-use-notify nil)
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(auto-save-list-file-prefix nil)
 '(backup-directory-alist `((".*" \, temporary-file-directory)))
 '(bookmark-default-file "/Users/idanko/.cache/emacs/bookmarks")
 '(c-basic-offset 4)
 '(comment-fill-column 80)
 '(company-posframe-quickhelp-show-header nil)
 '(company-posframe-show-indicator nil)
 '(company-posframe-show-metadata nil)
 '(company-posframe-show-params nil t)
 '(completion-category-defaults nil t)
 '(completion-category-overrides '((file (styles partial-completion))))
 '(completion-styles '(orderless basic))
 '(custom-safe-themes t)
 '(dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
 '(dashboard-items '((agenda . 8) (projects . 4) (recents . 4)))
 '(dashboard-set-footer nil)
 '(dired-dwim-target t)
 '(dired-omit-files "^\\...+$")
 '(dired-omit-verbose nil)
 '(display-line-numbers-type t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-stay-out-of '(flymake) t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(elfeed-search-filter "@6-months-ago +unread")
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(fill-column 80)
 '(format-all-default-formatters
   `(("YAML"
      (prettier ,my:config-path-prettier))
     ("JavaScript"
      (prettier ,my:config-path-prettier))))
 '(global-auto-revert-non-file-buffers t)
 '(gofmt-command "goimports")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 4)
 '(kill-whole-line t)
 '(magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
 '(mac-option-modifier 'control)
 '(mac-command-modifier 'meta)
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-hide-markup t)
 '(org-agenda-block-separator "")
 '(org-agenda-files nil)
 '(org-capture-bookmark nil)
 '(org-capture-templates
   `(("n" "[n]ew TODO item" entry
      (file org-default-notes-file)
      "* TODO %?
Entered on %U")))
 '(org-default-notes-file "/Users/idanko/github.com/illia-danko/org/todo.org")
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-emphasis-markers t)
 '(org-pretty-entities t)
 '(org-startup-indented t)
 '(org-superstar-leading-bullet 32)
 '(prefix-help-command #'embark-prefix-help-command t)
 '(projectile-known-projects-file "/Users/idanko/.cache/emacs/projectile-bookmarks.eld")
 '(python-indent-offset 4)
 '(recentf-save-file "/Users/idanko/.cache/emacs/recentf")
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(save-place-file "/Users/idanko/.cache/emacs/places")
 '(set-mark-command-repeat-pop t)
 '(tab-width 4)
 '(transient-history-file "/Users/idanko/.cache/emacs/transient/history.el")
 '(transient-levels-file "/Users/idanko/.cache/emacs/transient/levels.el")
 '(transient-values-file "/Users/idanko/.cache/emacs/transient/values.el")
 '(undohist-directory "/Users/idanko/.cache/emacs/undohist")
 '(url-configuration-directory "/Users/idanko/.cache/emacs/url")
 '(use-dialog-box nil)
 '(user-full-name "Elijah Danko")
 '(user-mail-address "me@elijahdanko.net")
 '(vc-follow-symlinks t)
 '(visible-cursor nil)
 '(xref-show-definitions-function 'consult-xref)
 '(xref-show-xrefs-function 'consult-xref))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Nerd Font Mono" :height 160))))
 '(fixed-pitch ((t (:family "Iosevka Nerd Font Mono" :height 160))))
 '(variable-pitch ((t (:family "Iosevka Nerd Font Mono" :height 160)))))

;;; settings.el ends here
