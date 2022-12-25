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

(defvar my:shared-directory "~/.cache/emacs"
  "Cloud file storage location.")

(defvar my:prettier-config-path (concat
                                 "--config="
                                 (expand-file-name "~/.config/prettier/prettier.config.js"))
  "Path to prettier config.")

(setq org-directory "~/github.com/illia-danko/org")

(unless (file-directory-p my:shared-directory)
  (make-directory my:shared-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/abbrevs.el")
 '(abbrev-mode t)                       ; use abbreviations everywhere
 '(auto-revert-check-vc-info t)
 '(auto-revert-use-notify nil)
 '(auto-revert-verbose nil)             ; dont print `Reverting buffer' message
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(auto-save-list-file-prefix nil)
 '(backup-directory-alist `((".*" . ,temporary-file-directory)))
 '(c-basic-offset 4)
 '(comment-fill-column 80)
 '(completion-category-defaults nil)
 '(completion-category-overrides '((file (styles . (orderless flex)))))
 '(completion-cycle-threshold 3) ; TAB cycle if there are only few candidates
 '(completion-styles '(orderless flex))
 '(corfu-auto t)
 '(corfu-preselect 'prompt)
 '(corfu-preview-current nil)
 '(custom-safe-themes t)        ; disable annoying warning while loading a theme
 '(dashboard-filter-agenda-entry 'dashboard-no-filter-agenda) ; show todo entries
 '(dashboard-items '((agenda . 8) (projects . 4) (recents . 4)))
 '(dashboard-set-footer nil)
 '(debug-on-error t)
 '(dired-dwim-target t)
 '(dired-omit-files "^\\...+$")         ; add hiden files to dired-omit-mode
 '(dired-omit-verbose nil)
 '(display-line-numbers-type t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-echo-area-use-multiline-p nil)
 '(elfeed-search-filter "@6-months-ago +unread")
 '(enable-local-variables :all) ; always trust .dir.locals.el (risk is accepted)
 '(enable-recursive-minibuffers t)
 '(evil-kill-on-visual-paste nil) ; paste on selection
 '(evil-undo-system 'undo-redo)
 '(evil-want-fine-undo t)
 '(evil-want-keybinding nil)
 '(evil-echo-state nil)
 '(evil-symbol-word-search t)
 '(fill-column 80)
 '(git-gutter:added-sign "")
 '(git-gutter:ask-p nil)
 '(git-gutter:deleted-sign "")
 '(git-gutter:modified-sign "")
 '(global-auto-revert-non-file-buffers t)
 '(gofmt-command "goimports")
 '(indent-tabs-mode nil)
 '(inhibit-splash-screen t)
 '(js-indent-level 4)
 '(kill-whole-line t)
 '(lsp-completion-provider :none)
 '(lsp-completion-show-detail nil)
 '(lsp-completion-show-kind t)
 '(lsp-diagnostic-package :none)
 '(lsp-diagnostics-modeline-scope nil) ;; Disable mode-line integration.
 '(lsp-eldoc-enable-hover nil)
 '(lsp-enable-links nil)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-headerline-breadcrumb-enable nil) ; Disable LSP headerline.
 '(lsp-lens-enable nil)
 '(lsp-modeline-diagnostics-enable nil)
 '(lsp-restart 'ignore) ; Suppress restart process confirmation.
 '(lsp-signature-auto-activate nil)
 '(lsp-signature-render-documentation nil)
 '(mac-command-modifier 'meta)
 '(mac-option-modifier 'control)
 '(magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
 '(markdown-fontify-code-blocks-natively t) ; highlight code block syntax
 '(markdown-hide-markup t)                  ; hide urls
 '(prefix-help-command #'embark-prefix-help-command)
 '(python-indent-offset 4)
 '(read-extended-command-predicate #'command-completion-default-include-p)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)          ; turn off ring bell sound
 '(set-mark-command-repeat-pop t) ; pop up mark in transitive way: don't repeat C-u C-SPC
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(use-dialog-box nil)
 '(user-full-name "Illia Danko")
 '(user-mail-address "illia@danko.ws")
 '(vc-follow-symlinks t)             ; turn off confirmation when open a symlink
 '(vertico-count 16)
 '(vertico-cycle t)
 '(vertico-resize nil)
 '(visible-cursor nil)               ; dont blink cursor in tty.
 '(wgrep-auto-save-buffer t)
 '(xref-show-definitions-function 'consult-xref)
 '(xref-show-xrefs-function 'consult-xref)
 `(bookmark-default-file ,(expand-file-name "bookmarks" my:shared-directory)) ; bookmarks path
 `(projectile-known-projects-file ,(expand-file-name "projectile-bookmarks.eld" my:shared-directory)) ; saved projects path
 `(recentf-save-file ,(expand-file-name "recentf" my:shared-directory)) ; recent files path
 `(save-place-file ,(expand-file-name "places" my:shared-directory)) ; saved file positions file path
 `(transient-history-file ,(expand-file-name "transient/history.el" my:shared-directory))
 `(transient-levels-file ,(expand-file-name "transient/levels.el" my:shared-directory)) ; transient-mode cache files
 `(transient-values-file ,(expand-file-name "transient/values.el" my:shared-directory))
 `(undohist-directory ,(expand-file-name "undohist" my:shared-directory))
 `(undohist-ignored-files '("COMMIT_EDITMSG"))
 `(url-configuration-directory ,(expand-file-name "url" my:shared-directory))
 )

(custom-set-variables
 '(format-all-default-formatters `(("YAML" (prettier ,my:prettier-config-path)) ("JavaScript" (prettier ,my:prettier-config-path)) ("Go" goimports) ("Emacs Lisp" emacs-lisp)))
 '(format-all-show-errors 'never)
 '(marginalia-command-categories '((flycheck-error-list-set-filter . builtin)
                                   (persp-switch-to-buffer . buffer)
                                   (projectile-find-file . project-file)
                                   (projectile-recentf . project-file)
                                   (projectile-switch-to-buffer . buffer)
                                   (projectile-switch-project . project-file)))
 )

(custom-set-variables
 '(org-agenda-block-separator "")
 '(org-agenda-files (list org-default-notes-file))
 '(org-capture-bookmark nil)            ; prevent storing bookmarks
 '(org-capture-templates `(("n" "[n]ew TODO item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")))
 '(org-ellipsis "  " )                 ; folding symbol
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-emphasis-markers t)    ; close links, etc.
 '(org-pretty-entities t)          ; show LaTeX-like symbols as UTF-8 characters
 '(org-startup-indented t)
 '(org-superstar-leading-bullet ?\s)
 `(org-default-notes-file ,(expand-file-name "todo.org" org-directory)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Nerd Font Mono" :height 160 :weight medium))))
 '(fixed-pitch ((t (:family "Iosevka Nerd Font Mono" :height 160 :weight medium))))
 '(variable-pitch ((t (:family "Iosevka Nerd Font Mono" :height 160 :weight medium)))))

;;; settings.el ends here
