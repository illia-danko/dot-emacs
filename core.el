;;; core.el --- Core Emacs configuration -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Elijah Danko
;;
;; Author: Elijah Danko <me@elijahdanko.net>
;; URL: https://github.com/elijahdanko/emacs.d

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
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1)) ; disable top menu buttons
(menu-bar-mode -1)
(blink-cursor-mode 0) ; gui mode


;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p) ; enable y/n answers
(auto-fill-mode 1) ; intelligently breaks content into lines
(delete-selection-mode t) ;; delete the selection with a key-press
(show-paren-mode 1) ; highlight matched parentheses
(global-auto-revert-mode 1) ; keep fresh changes even if changed outside

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
           ,(concat (getenv "HOME") "/github.com/elijahdanko/scripts"))))

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

(use-package abbrev)
(use-package eldoc)

(use-package hl-line
  :config
  :bind (("C-c t h" . hl-line-mode)))

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

(use-package simple
  :bind (("C-c C-z" . toggle-read-only)))

(use-package magit
  :straight t
  :after (project)
  :hook (git-commit-setup . flyspell-mode)
  :config
  (defun magit:push-notes ()
    "Stage, commit and push upstream a personal note file."
    (interactive)
    (let ((org-dir (expand-file-name org-directory))
          (fullname (buffer-file-name))
          (relname (file-name-nondirectory (buffer-file-name))))
      (when (string-match-p (regexp-quote org-dir) fullname)
        (magit-stage-file fullname)
        (magit-commit-create (list "-m" (format "Update %s" relname)))
        (call-interactively 'magit-push-current-to-upstream))))

  :bind (("C-c g g" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g d" . magit-diff-buffer-file)
         ("C-c g l" . magit-log-current)
         ("C-c g n" . magit:push-notes)))

;; (Emacs27.2).Fix "Symbol's value as variable is void" error in
;; `project-switch-commands'
(use-package project
  :straight t
  :ensure t)

(use-package git-link
  :straight t
  :config
  (defun git-link:open-homepage ()
    (interactive)
    (let ((git-link-open-in-browser t))
      (call-interactively 'git-link-homepage)))
  :bind (("C-c g u" . git-link)
         ("C-c g o" . git-link:open-homepage)))

 ; To sort M-x output.
(use-package smex :straight t)

(use-package counsel
  :straight t
  :ensure t
  :bind (("M-x" . counsel-M-x)
	     ("M-y" . counsel-yank-pop)
         ("C-c b" . counsel-bookmark)
         ("C-c r" . counsel-recentf)
	     ("C-c c" . counsel-compile)
         ("C-c x" . ivy-resume)
         ("C-c s" . (lambda ()
                      (interactive)
                      (region:apply 'counsel-rg)))
         ("C-c /" . counsel-imenu)))

(use-package ivy
  :straight t
  :config (ivy-mode 1))

(use-package ivy-rich
  :after (ivy)
  :straight t
  :config
  ;; Fix ivy Switch To Buffer slowness.
  (defvar ivy-rich:cache
    (make-hash-table :test 'equal))

  (defun ivy-rich:cache-lookup (delegate candidate)
    (let ((result (gethash candidate ivy-rich:cache)))
      (unless result
        (setq result (funcall delegate candidate))
        (puthash candidate result ivy-rich:cache))
      result))

  (defun ivy-rich:cache-reset ()
    (clrhash ivy-rich:cache))

  (defun ivy-rich:cache-rebuild ()
    (mapc (lambda (buffer)
            (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
          (buffer-list)))

  (defun ivy-rich:cache-rebuild-trigger ()
    (ivy-rich:cache-reset)
    (run-with-idle-timer 1 nil 'ivy-rich:cache-rebuild))

  (advice-add 'ivy-rich--ivy-switch-buffer-transformer :around 'ivy-rich:cache-lookup)
  (advice-add 'ivy-switch-buffer :after 'ivy-rich:cache-rebuild-trigger)

  (ivy-rich-mode 1))

(use-package flyspell-correct
  :straight t
  :after (flyspell ivy)
  :bind (:map flyspell-mode-map
              ("M-$" . flyspell-correct-wrapper)))

(use-package projectile
  :straight t
  :ensure t
  :config (projectile-mode 1)
  :bind (("C-c p" . projectile-switch-project)
         ("C-c f" . projectile-find-file)
         ("C-c #" . projectile-kill-buffers)))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode +1))

(use-package company
  :straight t
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
                company-backends))
  (add-to-list 'completion-styles 'initials t)  ; enabling fuzzy matching
  :bind (("C-c TAB" . company-complete)))

(use-package company-posframe
  :straight t
  :config
  (company-posframe-mode +1))

(use-package expand-region
  :straight t
  :bind (("C-o" . er/expand-region)
         ("M-o" . er/contract-region)))

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

  :hook (dired-mode . (lambda () (dired-hide-details-mode) (dired-omit-mode)))
  :bind (("C-x d" . dired-jump)
         :map dired-mode-map
         ("o" . dired:system-open)))

(use-package browse-url
  :bind (("C-c o" . browse-url-at-point)))

(use-package vterm :straight t)

(use-package isearch
  :init
  (defun isearch:region (&rest _)
    "If a region is active, set a selected pattern as an isearch input."
    (interactive "P\np")
    (if mark-active
	    (let ((content (region:content)))
		  (deactivate-mark)
		  (isearch-yank-string content))))
  :config
  (advice-add 'isearch-forward :after #'isearch:region)
  (advice-add 'isearch-backward :after #'isearch:region))

(use-package anzu
  :straight t
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (anzu-mode +1)
  (global-anzu-mode +1))

(use-package xclip
  :straight t
  :config
  (unless (display-graphic-p)
    (xclip-mode +1)))

(use-package display-line-numbers
  :bind (("C-c t l" . display-line-numbers-mode)))

(use-package code-review
  :straight t)

(use-package counsel-fzf-rg
  :straight '(counsel-fzf-rg
              :type git
              :host github
              :repo "elijahdanko/counsel-fzf-rg.el")
  :config
  (defun counsel-fzf-rg:org ()
    (interactive)
    (counsel-fzf-rg "" org-directory))
  :bind (("C-c n". counsel-fzf-rg:org)))

(use-package ttymux
  :straight '(ttymux
              :type git
              :host github
              :repo "elijahdanko/ttymux.el")
  :config
  (ttymux-mode 1))

 ;; Move buffer to the middle of the screen.
(use-package olivetti
  :straight t
  :init
  (defun distraction-free-toggle (&optional arg)
    (interactive)
    (call-interactively 'olivetti-mode arg)
    (call-interactively 'hide-mode-line-mode arg)))

(use-package hide-mode-line
  :straight t
  :init
  :after (olivetti)
  :bind (("C-c SPC" . distraction-free-toggle)))

(use-package elfeed
  :straight t
  :hook ((elfeed-show-mode . distraction-free-toggle))
  ;; Update elfeed database each 4 hours.
  :config (run-with-timer 0 (* 60 60 4) 'elfeed-update)
  :bind (("C-x f" . elfeed)))

(use-package undohist
  :straight t
  :config (undohist-initialize))

(use-package ediff-init
  :hook ((ediff-quit . delete-frame)))

(defun emacs:shutdown-server ()
  "Quit Emacs globally. Shutdown server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun edit:mark-line (&optional arg)
  "Mark current line. Shortcut of [C-a] [C-Space] [C-n]."
  (interactive)
  (move-beginning-of-line arg)
  (if (not mark-active)
      (set-mark-command arg))
  (next-logical-line 1 nil))

(defun edit:kill (&optional arg)
  "If mark active acts as `C-w' otherwise as `C-k'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (kill-line arg)))

(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-x !") #'emacs:shutdown-server)
(global-set-key (kbd "C-w") #'backward-kill-word)
(global-set-key (kbd "C-l") #'edit:mark-line)
(global-set-key [remap kill-line] #'edit:kill)

;;; core.el ends here
