;;; core.el --- Core Emacs configuration -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Illia A. Danko
;;
;; Author: Illia A. Danko <illia@idanko.net>
;; URL: https://github.com/idanko/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Enables basic configuration. It uses `straight.el' for loading packages from
;; the Internet. `use-package' in turns, for setup, configuring and memory
;; loading packages.

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

(blink-cursor-mode -1) ; the blinking cursor is nothing, but an annoyance
(tool-bar-mode -1) ; disable top menu buttons
(menu-bar-mode -1)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p) ; enable y/n answers
(auto-fill-mode 1) ; intelligently breaks content into lines
(delete-selection-mode t) ;; delete the selection with a key-press
(show-paren-mode 1) ; highlight matched parentheses
(global-auto-revert-mode 1) ; keep fresh changes even if changed outside

(global-hl-line-mode 1)
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

;; $PATH.
;;; Find executables used by packages.
(let ((path `("/usr/local/go/bin"
           ,(concat (getenv "HOME") "/go/bin")
           ,(concat (getenv "HOME") "/.fzf/bin")
           ,(concat (getenv "HOME") "/github.com/idanko/scripts"))))

  ;; To make $PATH works correctly on Emacs GUI it's needed to set via both:
  ;; `exec-path' and `setenv'.
  (setq exec-path (append exec-path path))

  (mapc (lambda (p)
          (setenv "PATH" (concat (getenv "PATH") ":" p)))
        path))

;; Bootstrap package manager.
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; Common functions.
(defun region:content ()
  "Takes region content if any."
  (buffer-substring-no-properties (mark) (point)))

(defun region:apply (fn)
  "Apply fn to the marked region text."
  (interactive)
  (if mark-active
	  (let ((content (region:content)))
		(deactivate-mark)
		(funcall fn content))
	(funcall fn)))

(require 'use-package)

(use-package diminish :straight t) ; `use-package' internals.

(use-package use-package-chords ; `use-package' internals.
  :straight t
  :ensure t
  :config (key-chord-mode 1))

 ; Removed from modeline.
(use-package abbrev :diminish)
(use-package eldoc :diminish)

(use-package simple
  :chords (("JJ" . delete-indentation)))

(use-package flyspell
  :init
  (defun flyspell:toggle ()
    "Toggle spell checking."
    (interactive)
    (if flyspell-mode
        (progn
          (message "Spell checking off")
          (flyspell-mode -1))
      (progn
        (message "Spell checking on")
        (if (derived-mode-p 'prog-mode)
            (flyspell-prog-mode)
          (flyspell-mode +1))
        (flyspell-buffer))))
  :bind (("C-c t s" . flyspell:toggle)))

(use-package magit
  :straight t
  :hook (git-commit-setup . flyspell-mode)
  :bind (("C-c g g" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g d" . magit-diff-buffer-file)
         ("C-c g l" . magit-log)))

(use-package git-link
  :straight t
  :bind (("C-c g u" . git-link)))

 ; To sort M-x output.
(use-package smex :straight t)

(use-package counsel
  :straight t
  :ensure t
  :bind (("M-x" . counsel-M-x)
	     ("M-y" . counsel-yank-pop)
         ("C-c f f" . counsel-bookmark)
         ("C-c f r" . counsel-recentf)
	     ("C-c c !" . counsel-compile)
         ("C-c c r" . recompile) ; not a part of counsel, but used as a supplement to `counsel-compile'
         ("C-s" . (lambda ()
                    (interactive)
                    (region:apply 'swiper-isearch)))
         ("C-r" . (lambda ()
                    (interactive)
                    (region:apply 'swiper-isearch-backward))))
  :chords (("RR" . ivy-resume)
           ("FF" . (lambda ()
                     (interactive)
                     (region:apply 'counsel-rg)))))

(use-package ivy
  :straight t
  :diminish
  :config (ivy-mode 1))

(use-package ivy-rich
  :after (ivy)
  :straight t
  :config (ivy-rich-mode 1))

(use-package projectile
  :straight t
  :ensure t
  :config (projectile-mode 1)
  :bind (("C-c p" . projectile-command-map)
	     :map projectile-command-map
	     ("#" . projectile-remove-known-project)))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1))

(use-package company
  :straight t
  :diminish
  :after (yasnippet)
  :config
  (global-company-mode +1)

  ;; Use yasnippet in company.
  (setq company-backends
        (mapcar (lambda (backends)
                  (if (and (listp backends) (memq 'company-yasnippet backends))
                      backends
                    (append (if (consp backends)
    	                        backends
                              (list backends))
                            '(:with company-yasnippet))))
                company-backends)))

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))


(use-package eglot :straight t)
(use-package flycheck :straight t)
(use-package rg :straight t)
(use-package hydra :straight t)

(use-package multiple-cursors
  :straight t
  :after (hydra)
  :init
  (defhydra mc:hydra-keymap (:color blue)
    "Multiple Cursors"
    (">" mc/mark-next-like-this "mark next" :exit nil)
    ("n" mc/skip-to-next-like-this "skip to next" :exit nil)
    ("<" mc/unmark-next-like-this "unmark" :exit nil)
    ("m" mc/edit-lines "edit selection" :exit t)
    ("a" mc/mark-all-like-this "mark all" :exit t)
    ("q" nil "cancel"))
  :bind (("C-c m" . mc:hydra-keymap/body)))

;; Other settings.
(defun emacs:shutdown-server ()
  "Quit Emacs globally. Shutdown server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-c b r") #'revert-buffer)
(global-set-key (kbd "C-x !") #'emacs:shutdown-server)

(use-package dired
  :init
  (defun dired:system-open ()
    "Open in dired.
https://www.emacswiki.org/emacs/OperatingOnFilesInDired"
    (interactive)
    (let ((file (dired-get-filename nil t))
          (cmd (pcase system-type
                 ('darwin "open")
                 (_ "xdg-open"))))
      (call-process cmd nil 0 nil file)))

  :hook (dired-mode . dired-hide-details-mode)
  :bind (("C-x d" . dired-jump)
         :map dired-mode-map
         ("o" . dired:system-open)))

(use-package browse-url
  :bind (("C-c o w" . browse-url-at-point)))

(use-package eshell
  :bind (("C-c o t" . eshell)))

(use-package vterm :straight t
  :bind (("C-c o T" . vterm))
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil))))

;;; core.el ends here
