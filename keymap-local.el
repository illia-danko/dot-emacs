;;; keymap-local.el --- Local Per Mode Shortcuts -*- lexical-binding: t -*-

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

(add-hook
 'projectile-mode-hook
 (lambda nil
   (define-key projectile-command-map "#" #'projectile-kill-buffers)
   (define-key projectile-command-map "!" #'projectile-remove-known-project)
   (define-key projectile-command-map "f" #'consult-find)))

(add-hook
 'eglot-managed-mode-hook
 (lambda nil
   (evil-define-key 'normal eglot-mode-map (kbd "SPC mr") #'eglot-rename)
   (evil-define-key 'normal eglot-mode-map (kbd "SPC mi") #'eglot-find-implementation)))

(add-hook
 'emacs-lisp-mode-hook
 (lambda nil
   (evil-define-key 'normal emacs-lisp-mode-map (kbd ",ee") #'eval-last-sexp)
   (evil-define-key 'normal emacs-lisp-mode-map (kbd ",ew") #'eval-defun)
   (evil-define-key 'normal emacs-lisp-mode-map (kbd ",eb") #'eval-buffer)))

(add-hook
 'dired-mode-hook
 (lambda nil
   (evil-collection-define-key 'normal 'dired-mode-map
     "o" '#'u:system-open)))

(add-hook
 'go-mode-hook
 (lambda nil
   (evil-define-key 'normal go-mode-map (kbd "SPC mt") #'go-test-current-test)
   (evil-define-key 'normal go-mode-map (kbd "SPC mb") #'go-test-current-file)))

(add-hook
 'paredit-mode-hook
 (lambda nil
   (evil-define-key 'normal paredit-mode-map (kbd ",ed") #'paredit-splice-sexp-killing-backward)
   (evil-define-key 'normal paredit-mode-map (kbd ",e>") #'paredit-forward-slurp-sexp)
   (evil-define-key 'normal paredit-mode-map (kbd ",e<") #'paredit-forward-barf-sexp)))

(add-hook
 'markdown-mode-hook
 (lambda nil
   (evil-define-key 'normal markdown-mode-map (kbd "SPC m *") #'u:markdown-toggle-fontifications)))

(add-hook
 'org-mode-hook
 (lambda nil
   (evil-define-key 'normal org-mode-map (kbd "SPC m *") #'u:org-toggle-fontifications)))

(add-hook
 'evil-mode-hook
 (lambda nil
   (define-key evil-normal-state-map   (kbd "C-g") #'u:evil-keyboard-quit)
   (define-key evil-motion-state-map   (kbd "C-g") #'u:evil-keyboard-quit)
   (define-key evil-insert-state-map   (kbd "C-g") #'u:evil-keyboard-quit)
   (define-key evil-window-map         (kbd "C-g") #'u:evil-keyboard-quit)
   (define-key evil-operator-state-map (kbd "C-g") #'u:evil-keyboard-quit)))

;;; keymap-local.el ends here