;;; packages.el --- 3-rd party packages  -*- lexical-binding: t -*-

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

(require 'use-package)

(use-package undo-fu :straight t)

(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-fu)
  :config (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))
(use-package magit :straight t :after (project) :hook (git-commit-setup . flyspell-mode))
(use-package git-link :straight t)
(use-package vertico :straight t :config (vertico-mode 1))
(use-package orderless :straight t)
(use-package savehist :init (savehist-mode))  ;; save minibuffer history
(use-package marginalia :straight t :config (marginalia-mode))
(use-package consult :straight t)
(use-package embark-consult :straight t)
(use-package projectile :straight t :ensure t :config (projectile-mode 1))
(use-package yasnippet :straight t :config (yas-global-mode +1))
(use-package company-posframe :straight t :config (company-posframe-mode +1))
(use-package company :straight t :after (yasnippet company-posframe) :config (global-company-mode +1))
(use-package expand-region :straight t)
(use-package eglot :straight t)
(use-package flycheck :straight t)
(use-package rg :straight t)
(use-package olivetti :straight t)
(use-package hide-mode-line :straight t :init :after (olivetti))
(use-package dired :hook (dired-mode . dired-mode:hook))
(use-package anzu :straight t :config (anzu-mode +1) (global-anzu-mode +1))
(use-package xclip :straight t :config (unless (display-graphic-p) (xclip-mode +1)))
(use-package undohist :straight t :config (undohist-initialize))
(use-package ediff-init :hook ((ediff-quit . delete-frame)))
(use-package vterm :straight t :hook ((vterm-mode . hide-mode-line-mode)))
(use-package prog-mode :hook ((prog-mode . prog-mode:hook)))
(use-package go-mode :straight t :init :hook ((go-mode . go-mode:hook)))
(use-package go-test :straight t :defer t)
(use-package rainbow-delimiters :straight t)
(use-package paredit :straight t)
(use-package format-all :straight t :hook ((format-all-mode . format-all-ensure-formatter)))
(use-package elisp-mode :hook ((emacs-lisp-mode . (lambda () (paredit-mode) (rainbow-delimiters-mode)))))
(use-package cider :straight t :hook ((clojure-mode . clojure-mode:hook)))
(use-package typescript-mode :straight t)
(use-package js :hook ((js-mode . js-mode:hook)))
(use-package yaml-mode :straight t :hook ((yaml-mode . yaml-mode:hook)))
(use-package conf-mode :hook ((conf-space-mode . prog-mode:hook)))
(use-package protobuf-mode :straight t)
(use-package dockerfile-mode :straight t :hook ((docker-mode . prog-mode:hook)))
(use-package markdown-mode :straight t :hook ((markdown-mode . trailing-whitespace:show)))
(use-package org-superstar :straight t)
(use-package htmlize :straight t)
(use-package python :init :hook ((python-mode . python-mode:hook)))
(use-package sh-script :hook ((sh-mode . sh-mode:hook)))
(use-package restclient :straight t :mode ("\\.http\\'" . restclient-mode))
(use-package doom-themes :straight t)
(use-package which-key :straight t :config (which-key-mode 1))
(use-package doom-modeline :straight t :config (doom-modeline-mode 1))
(use-package rainbow-mode :straight t)
(use-package dashboard :straight t :config (dashboard-setup-startup-hook))

(use-package org
  :hook ((org-mode . (lambda ()
                       (org:fixup-electric-pairs)
                       (org-superstar-mode)
                       (trailing-whitespace:show)))))

(use-package isearch
  :init
  :config
  (advice-add 'isearch-forward :after #'isearch:region)
  (advice-add 'isearch-backward :after #'isearch:region))

(use-package ttymux
  :straight '(ttymux
              :type git
              :host github
              :repo "illia-danko/ttymux.el")
  :config
  (ttymux-mode 1))

(use-package elfeed
  :straight t
  :hook ((elfeed-show-mode . distraction-free-toggle))
  ;; Update elfeed database each 4 hours.
  :config (run-with-timer 0 (* 60 60 4) 'elfeed-update))

;;; packages.el ends here
