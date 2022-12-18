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

(use-package evil :straight t
  :demand t
  :init
  ;; Define high precedence map (See evil-collection/README.md).
  (defvar my-intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")

  (define-minor-mode my-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)

  (my-intercept-mode 1)

  (defun my-evil-keyboard-quit ()
    "Keyboard quit and force normal state."
    (interactive)
    (and evil-mode (evil-force-normal-state))
    (keyboard-quit))

  :config
  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
     state))

  (define-key evil-normal-state-map   (kbd "C-g") #'my-evil-keyboard-quit)
  (define-key evil-motion-state-map   (kbd "C-g") #'my-evil-keyboard-quit)
  (define-key evil-insert-state-map   (kbd "C-g") #'my-evil-keyboard-quit)
  (define-key evil-window-map         (kbd "C-g") #'my-evil-keyboard-quit)
  (define-key evil-operator-state-map (kbd "C-g") #'my-evil-keyboard-quit)

  (evil-define-key* 'normal my-intercept-mode-map
    (kbd "SPC :") #'execute-extended-command
    (kbd "SPC ;") #'eval-expression)

  (evil-mode 1))

(use-package evil-collection :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary :straight t
  :config
  (evil-commentary-mode 1))

(use-package evil-terminal-cursor-changer :straight t
  :ensure t
  :config
  (unless (display-graphic-p)
    (etcc-on)))

(use-package corfu :straight t
  :config
  (global-corfu-mode 1))

(use-package corfu-terminal :straight t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

(use-package cape :straight t           ; complection backend for corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package orderless :straight t)

(use-package help
  :config
  (evil-define-key* 'normal my-intercept-mode-map
    (kbd "SPC hk") #'describe-key
    (kbd "SPC hw") #'where-is
    (kbd "SPC hv") #'describe-variable
    (kbd "SPC hv") #'describe-variable
    (kbd "SPC hf") #'describe-function
    "g?" #'describe-mode))

(use-package magit :straight t
  :after (project))

(use-package git-link :straight t)
(use-package vertico :straight t
  ;; :hook (minibuffer-setup . vertico-repeat-save)
  :config (vertico-mode 1))
;; (use-package vertico-posframe :straight t
;;   :config (vertico-posframe-mode 1)
;;   (when (display-graphic-p)
;;     (vertico-posframe-mode 1)))

(use-package marginalia :straight t
  :config (marginalia-mode))

(use-package consult :straight t
  :config
  (defun my-consult-ripgrep-region ()
    "Apply fn to the marked region text."
    (interactive)
    (if mark-active
        (let ((content (buffer-substring-no-properties (mark) (point))))
          (deactivate-mark)
          (consult-ripgrep nil content))
      (consult-ripgrep)))

  (global-set-key [remap apropos]                       #'consult-apropos)
  (global-set-key [remap bookmark-jump]                 #'consult-bookmark)
  (global-set-key [remap evil-show-marks]               #'consult-mark)
  (global-set-key [remap evil-show-registers]           #'consult-register)
  (global-set-key [remap goto-line]                     #'consult-goto-line)
  (global-set-key [remap imenu]                         #'consult-imenu)
  (global-set-key [remap locate]                        #'consult-locate)
  (global-set-key [remap load-theme]                    #'consult-theme)
  (global-set-key [remap man]                           #'consult-man)
  (global-set-key [remap recentf-open-files]            #'consult-recent-file)
  (global-set-key [remap switch-to-buffer]              #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
  (global-set-key [remap yank-pop]                      #'consult-yank-pop)

  (evil-define-key* 'normal my-intercept-mode-map
    (kbd "SPC <") #'consult-buffer)
  (evil-define-key* '(normal visual) my-intercept-mode-map
    (kbd "SPC /") #'my-consult-ripgrep-region))

(use-package embark-consult :straight t)

(use-package savehist
  :init (savehist-mode))  ;; save minibuffer history

(use-package projectile :straight t
  :ensure t
  :config
  (define-key projectile-command-map "#" #'projectile-kill-buffers)
  (define-key projectile-command-map "!" #'projectile-remove-known-project)
  (evil-define-key* 'normal my-intercept-mode-map
    (kbd "SPC p") #'projectile-command-map)

  (projectile-mode 1))

(use-package yasnippet :straight t :config (yas-global-mode +1))
(use-package expand-region :straight t)

(use-package eglot :straight t
  :init
  (defun my-eglot-ensure ()
    (unless (eq major-mode 'ediff-mode)
      (eglot-ensure)))

  :hook ((go-mode . my-eglot-ensure))

  :config
  (evil-define-key* 'normal eglot-mode-map
    ",cr" #'eglot-rename
    ",ci" #'eglot-find-implementation))

(use-package flyspell
  :hook (git-commit-setup . flyspell-mode))

(use-package flycheck :straight t
  :init
  (defun my-flycheck-mode ()
    (unless (eq major-mode 'ediff-mode)
      (flycheck-mode)))

  :hook ((go-mode . my-flycheck-mode)))

(use-package rg :straight t)
(use-package olivetti :straight t)
(use-package hide-mode-line :straight t :init :after (olivetti))

(use-package dired
  :init
  (defun my-system-open ()
    (interactive)
    (let ((file (dired-get-filename nil t))
          (cmd (pcase system-type
                 ('darwin "open")
                 (_ "xdg-open"))))
      (call-process cmd nil 0 nil file)))

  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)

  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "o" #'my-system-open)
  (evil-define-key* 'normal my-intercept-mode-map
    "-" #'dired-jump))

(use-package anzu :straight t
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode 1))

(use-package xclip :straight t :config (unless (display-graphic-p) (xclip-mode +1)))
(use-package undohist :straight t :config (undohist-initialize))
(use-package ediff-init :hook ((ediff-quit . delete-frame)))
(use-package vterm :straight t :hook ((vterm-mode . hide-mode-line-mode)))
(use-package prog-mode :hook ((prog-mode . intern:prog-mode-hook)))

(use-package go-mode :straight t
  :hook ((go-mode . (lambda nil
                      (setq-local comment-fill-column 150
                                  fill-column 150))))
  :config
  (evil-define-key* 'normal go-mode-map
    "gd" nil ; do not override lsp `go-to-definition'.
    ))

(use-package rainbow-delimiters :straight t
  :hook ((emacs-lisp-mode clojure-mode) . rainbow-delimiters-mode))

(use-package format-all :straight t
  :hook ((emacs-lisp-mode clojure-mode go-mode) . format-all-mode)
  :hook ((format-all-mode . format-all-ensure-formatter)))

(use-package typescript-mode :straight t)
(use-package js :hook ((js-mode . intern:js-mode-hook)))
(use-package yaml-mode :straight t :hook ((yaml-mode . intern:yaml-mode-hook)))
(use-package conf-mode :hook ((conf-space-mode . intern:prog-mode-hook)))
(use-package protobuf-mode :straight t)
(use-package dockerfile-mode :straight t :hook ((docker-mode . intern:prog-mode-hook)))

(use-package markdown-mode :straight t
  :init
  (defun my-markdown-toggle-fontifications (&optional arg)
    "Toggle fontifications on/off."
    (interactive (list (or current-prefix-arg 'toggle)))
    (markdown-toggle-markup-hiding arg))

  :hook (markdown-mode  . my-markdown-toggle-fontifications)
  :config
  (define-key markdown-mode-map (kbd "C-c *") #'my-markdown-toggle-fontifications))

(use-package python :init :hook ((python-mode . intern:python-mode-hook)))
(use-package sh-script :hook ((sh-mode . intern:sh-mode-hook)))
(use-package restclient :straight t :mode ("\\.http\\'" . restclient-mode))
(use-package doom-themes :straight t)
(use-package which-key :straight t :config (which-key-mode 1))
(use-package doom-modeline :straight t :config (doom-modeline-mode 1))
(use-package rainbow-mode :straight t)
(use-package dashboard :straight t :config (dashboard-setup-startup-hook))

(use-package org
  :init
  (defun my-org-toggle-fontifications ()
    "Toggle fontifications on/off.
The solution taken from
https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el#L3037-L3069"
    (interactive)
    ;; Toggle markers.
    (setq-local org-hide-emphasis-markers
                (not org-hide-emphasis-markers))
    ;; Toggle links.
    (if org-link-descriptive
        (remove-from-invisibility-spec '(org-link))
      (add-to-invisibility-spec '(org-link)))
    (setq-local org-link-descriptive
                (not org-link-descriptive))
    ;; Apply changes.
    (font-lock-fontify-buffer))

  :hook ((org-mode . (lambda ()
                       (org-superstar-mode)
                       (setq-local show-trailing-whitespace t))))
  :config
  (define-key org-mode-map (kbd "C-c *") #'my-org-toggle-fontifications))

(use-package org-superstar :straight t)

(use-package isearch
  :config
  (advice-add 'isearch-forward :after #'intern:isearch-region)
  (advice-add 'isearch-backward :after #'intern:isearch-region))

(use-package navigate
  :after evil
  :straight '(navigate
              :type git
              :host github
              :repo "keith/evil-tmux-navigator"))

(use-package elfeed :straight t
  :hook ((elfeed-show-mode . intern:zen-toggle))
  ;; Update elfeed database each 4 hours.
  :config (run-with-timer 0 (* 60 60 4) 'elfeed-update))

(use-package elisp-mode
  :config
  (evil-define-key* 'normal emacs-lisp-mode-map
    ",ee" #'eval-last-sexp
    ",ew" #'eval-defun
    ",eb" #'eval-buffer))

(use-package cider :straight t
  :config
  (evil-define-key* '(normal visual) cider-mode-map
    "K" #'cider-clojuredocs)
  (evil-define-key* 'normal cider-mode-map
    ",ee" #'cider-eval-sexp-at-point
    ",ew" #'cider-eval-defun-at-point
    ",eb" #'cider-eval-buffer
    ",cn" #'cider-repl-set-ns))

(use-package cider-repl
  :config
  (evil-define-key* '(normal visual) cider-repl-mode-map
    "K" #'cider-clojuredocs))

(use-package cider-macroexpansion
  :config
  (evil-define-key* '(normal) cider-macroexpansion-mode-map
    "K" #'cider-clojuredocs))

(use-package clojure-mode
  :config
  (evil-define-key* 'normal clojure-mode-map
    ",cc" #'cider-jack-in))

(use-package clj-refactor :straight t
  :hook ((clojure-mode . clj-refactor-mode)))

(use-package paredit :straight t
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

(use-package eldoc
  :config
  (global-eldoc-mode -1))

;;; packages.el ends here
