;;; keymap-global.el --- Global Shortcuts -*- lexical-binding: t -*-

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

(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; Define high precedence map (See evil-collection/README.md).
(defvar user:intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode user:intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(user:intercept-mode +1)

(dolist (state '(normal visual insert))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap user:intercept-mode-map state t t)
   state))

;;;; Mapping.

;; Navigation & Search.
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC p") #'projectile-command-map)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC <") #'consult-buffer)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC :") #'execute-extended-command)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC ;") #'eval-expression)
(evil-define-key '(normal visual) user:intercept-mode-map (kbd "SPC /") #'(lambda () (interactive) (region:apply 'consult-ripgrep)))
(evil-define-key 'normal user:intercept-mode-map "-" #'dired-jump)

;; Help.
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC hk") #'describe-key)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC hw") #'where-is)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC hv") #'describe-variable)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC hf") #'describe-function)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC hm") #'describe-mode)

;; Exit & Enter Emacs.
(evil-define-key 'normal user:intercept-mode-map (kbd ",qq") #'emacs:shutdown-server)

;; Toggle.
(evil-define-key 'normal user:intercept-mode-map (kbd ",th") #'hl-line-mode)
(evil-define-key 'normal user:intercept-mode-map (kbd ",ts") #'flyspell:toggle)
(evil-define-key 'normal user:intercept-mode-map (kbd ",tr") #'read-only-mode)
(evil-define-key 'normal user:intercept-mode-map (kbd ",tn") #'display-line-numbers-mode)
(evil-define-key 'normal user:intercept-mode-map (kbd ",tc") #'rainbow-mode)
(evil-define-key 'normal user:intercept-mode-map (kbd ",tz") #'user:zen-toggle)

;; Git & version control.
(evil-define-key 'normal user:intercept-mode-map (kbd ",gg") #'magit-status)
(evil-define-key 'normal user:intercept-mode-map (kbd ",g?") #'magit-blame-addition)
(evil-define-key 'normal user:intercept-mode-map (kbd ",gL") #'magit-log-all)
(evil-define-key 'normal user:intercept-mode-map (kbd ",gl") #'magit-log-buffer-file)
(evil-define-key 'normal user:intercept-mode-map (kbd ",gc") #'vc:push)
(evil-define-key 'normal user:intercept-mode-map (kbd ",gu") #'git-link)
(evil-define-key 'normal user:intercept-mode-map (kbd ",gU") #'git-link:open-homepage)

;; Edit.
(evil-define-key '(normal insert) user:intercept-mode-map (kbd "C-y") #'consult-yank-from-kill-ring)
(evil-define-key 'normal user:intercept-mode-map (kbd "C-t") #'er/expand-region)
(evil-define-key 'normal user:intercept-mode-map (kbd "C-q") #'er/contract-region)

;; Files & Buffers.
(evil-define-key 'normal user:intercept-mode-map (kbd ",fr") #'consult-recent-file)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC mm") #'consult-imenu)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC ms") #'wgrep-save-all-buffers)

;; Open.
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC of") #'elfeed)

;; Notes.
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC ns") #'(lambda () (interactive) (consult-ripgrep org-directory)))
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC nt") #'org:new-todo-entry)
(evil-define-key 'normal user:intercept-mode-map (kbd "SPC nl") #'org-todo-list)

;;; keymap-global.el ends here
