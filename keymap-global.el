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

;;;; Mapping.

;; Navigation & Search.
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC p") #'projectile-command-map)
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC <") #'consult-buffer)
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC :") #'execute-extended-command)
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC ;") #'eval-expression)
;; (evil-define-key '(normal visual) intern:intercept-mode-map (kbd "SPC /") #'(lambda () (interactive) (intern:region-apply 'consult-ripgrep)))
;; (evil-define-key 'normal intern:intercept-mode-map "-" #'dired-jump)

;; Help.
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC hk") #'describe-key)
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC hw") #'where-is)
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC hv") #'describe-variable)
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC hf") #'describe-function)
;; (evil-define-key 'normal intern:intercept-mode-map (kbd "SPC hm") #'describe-mode)

;; Exit & Enter Emacs.

;; Toggle.
(evil-define-key 'normal intern:intercept-mode-map (kbd ",th") #'hl-line-mode)
(evil-define-key 'normal intern:intercept-mode-map (kbd ",ts") #'intern:flyspell-toggle)
(evil-define-key 'normal intern:intercept-mode-map (kbd ",tr") #'read-only-mode)
(evil-define-key 'normal intern:intercept-mode-map (kbd ",tn") #'display-line-numbers-mode)
(evil-define-key 'normal intern:intercept-mode-map (kbd ",tc") #'rainbow-mode)

;; Git & version control.

;; Open.
;; Notes.
(evil-define-key 'normal intern:intercept-mode-map (kbd "SPC ns") #'(lambda () (interactive) (consult-ripgrep org-directory)))
(evil-define-key 'normal intern:intercept-mode-map (kbd "SPC nt") #'intern:org-new-todo-entry)
(evil-define-key 'normal intern:intercept-mode-map (kbd "SPC nl") #'org-todo-list)

;;; keymap-global.el ends here
