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

(defvar time-emacs-start (current-time)
  "Time Emacs has started.")

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      load-prefer-newer t
      font-lock-verbose nil
      byte-compile-verbose nil)

(add-hook 'emacs-startup-hook
	      (lambda ()
	        ;; After startup, it is important you reset this to some
            ;; reasonable default. A large gc-cons-threshold will
            ;; cause freezing and stuttering during long-term
            ;; interactive use."
            (setq gc-cons-threshold 16777216
		          gc-cons-percentage 0.1)))

(setq load-path
      (append (delete-dups load-path)
              `(,(expand-file-name "lisp" user-emacs-directory))))

(defvar my:shared-directory "~/.cache/emacs"
  "Cloud file storage location.")

(setq custom-file (expand-file-name "settings.el" user-emacs-directory))
(load custom-file)
(let ((private-settings (expand-file-name "private-settings.el" my:shared-directory)))
  (and private-settings
       (file-exists-p private-settings)
       (load private-settings)))

(let ((path `("/usr/local/go/bin"
              "/opt/homebrew/bin"
              ,(concat (getenv "HOME") "/go/bin"))))
  ;; To make $PATH works correctly on Emacs GUI it's needed to set via both:
  ;; `exec-path' and `setenv'.
  (setq exec-path (append exec-path path))
  (mapc (lambda (p)
          (setenv "PATH" (concat (getenv "PATH") ":" p)))
        path))

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
  (defvar my:intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")

  (define-minor-mode my:intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)

  (my:intercept-mode 1)

  (defun my:evil-keyboard-quit ()
    "Keyboard quit and force normal state."
    (interactive)
    (and evil-mode (evil-force-normal-state))
    (keyboard-quit))

  :config
  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap my:intercept-mode-map state t t)
     state))

  (define-key evil-normal-state-map   (kbd "C-g") #'my:evil-keyboard-quit)
  (define-key evil-motion-state-map   (kbd "C-g") #'my:evil-keyboard-quit)
  (define-key evil-insert-state-map   (kbd "C-g") #'my:evil-keyboard-quit)
  (define-key evil-window-map         (kbd "C-g") #'my:evil-keyboard-quit)
  (define-key evil-operator-state-map (kbd "C-g") #'my:evil-keyboard-quit)

  ;; Vim command line (echo area).
  (define-key evil-ex-completion-map (kbd "C-b") #'backward-char)
  (define-key evil-ex-completion-map (kbd "C-f") #'forward-char)
  (define-key evil-ex-completion-map (kbd "C-a") #'beginning-of-line)

  (modify-syntax-entry ?_ "w") ; used along with (setq evil-symbol-word-search t)

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

(use-package evil-anzu :straight t
  :after (evil)
  :ensure t)

(use-package anzu :straight t
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode 1))

(use-package corfu :straight t
  :config
  (global-corfu-mode 1))

(use-package corfu-terminal :straight t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

(use-package yasnippet :straight t
  :ensure t
  :config (yas-global-mode 1))

(use-package company :straight t) ; used by my:snippet-capf

(use-package cape :straight t           ; complection backend for corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)

  (defun my:snippet-capf ()
    (setq-local completion-at-point-functions
                (cons (cape-company-to-capf 'company-yasnippet) completion-at-point-functions)))

  :hook ((lsp-configure text-mode emacs-lisp-mode) . my:snippet-capf))

(use-package orderless :straight t)

(use-package vertico :straight t
  :config (vertico-mode 1))

(use-package marginalia :straight t
  :config (marginalia-mode))

(use-package recentf
  :config
  (recentf-mode))

(use-package consult :straight t
  :config
  (defun my:consult-ripgrep-region ()
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

  (evil-define-key* '(normal visual) my:intercept-mode-map
    (kbd "SPC /") #'my:consult-ripgrep-region)

  (evil-define-key* '(normal insert) my:intercept-mode-map
    (kbd "C-y") #'consult-yank-from-kill-ring)

  (evil-define-key* 'normal my:intercept-mode-map
    (kbd "SPC <") #'consult-buffer
    ",fr" #'consult-recent-file
    (kbd "gm") #'consult-imenu
    (kbd "SPC os") #'(lambda nil
                       (interactive)
                       (consult-ripgrep org-directory))))

(use-package embark-consult :straight t
  :config
  (evil-define-key* nil my:intercept-mode-map
    (kbd "C-d") #'embark-act))

(use-package embark :straight t)

(use-package help
  :config
  (evil-define-key* 'normal my:intercept-mode-map
    (kbd ",dk") #'describe-key
    (kbd ",dw") #'where-is
    (kbd ",dv") #'describe-variable
    (kbd ",df") #'describe-function
    "g?" #'describe-mode))

(use-package magit :straight t
  :init
  (defun my:git-push-buffer-update ()
    "Stage, commit and push upstream a personal note file."
    (interactive)
    (let* ((fullname (buffer-file-name))
           (relname (file-name-nondirectory fullname)))
      (call-process "git" nil nil nil "add" fullname)
      (call-process "git" nil nil nil "commit" "-m" (format "Update %s" relname))
      (call-process "git" nil nil nil "push")
      (message "Pushed %s" relname)))

  :config
  (evil-define-key* 'normal my:intercept-mode-map
    ",gg" #'magit-status
    ",g?" #'magit-blame-addition
    ",gd" #'magit-diff-buffer-file
    ",gl" #'magit-log-all
    ",gb" #'magit-log-buffer-file
    ",gc" #'my:git-push-buffer-update))

(use-package git-commit
  :after (magit)
  :hook (git-commit-setup . evil-insert-state))

(use-package git-link :straight t
  :init
  (defun my:git-link-open-page ()
    (interactive)
    (let ((git-link-open-in-browser t))
      (call-interactively 'git-link-homepage)))

  :config
  (evil-define-key* 'normal my:intercept-mode-map
    ",gu" #'git-link
    ",gU" #'my:git-link-open-page))

(use-package git-gutter :straight t
  :init
  (defun my:git-gutter-refresh-hunks (&rest args)
    (interactive)
    (git-gutter:update-all-windows))

  (defun my:git-gutter-enable (&rest args)
    (interactive)
    (git-gutter-mode 1))

  (defun my:git-gutter-popup-hunk-jump (&optional diffinfo)
    (interactive)
    (git-gutter:popup-hunk diffinfo)
    (switch-to-buffer-other-window git-gutter:popup-buffer))

  :hook ((after-change-major-mode . my:git-gutter-enable))
  :config
  (advice-add 'find-file :after #'my:git-gutter-refresh-hunks)
  (advice-add 'pop-to-buffer-same-window :after #'my:git-gutter-refresh-hunks)
  (advice-add 'switch-to-buffer :after #'my:git-gutter-refresh-hunks)
  (advice-add 'switch-to-buffer-other-window :after #'my:git-gutter-refresh-hunks)

  (evil-define-key* 'normal my:intercept-mode-map
    (kbd "]c") #'git-gutter:next-hunk
    (kbd "[c") #'git-gutter:previous-hunk
    (kbd "SPC hu") #'git-gutter:revert-hunk
    (kbd "SPC hp") #'my:git-gutter-popup-hunk-jump)

  (global-git-gutter-mode 1))

(use-package savehist
  :init (savehist-mode))  ;; save minibuffer history

(use-package projectile :straight t
  :ensure t
  :config
  (define-key projectile-command-map "d" #'projectile-kill-buffers)
  (define-key projectile-command-map "#" #'projectile-remove-known-project)
  (evil-define-key* 'normal my:intercept-mode-map
    (kbd "SPC p") #'projectile-command-map)

  (projectile-mode 1))

(use-package lsp-mode :straight t
  :init
  (defun my:lsp-deferred ()
    (unless (eq major-mode 'ediff-mode)
      (lsp-deferred)))

  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ((go-mode js-mode python-mode) . my:lsp-deferred)

  :config
  (evil-define-key* 'normal lsp-mode-map
    ",cr" #'lsp-rename
    ",cu" #'lsp-find-references
    ",cd" #'lsp-restart-workspace
    ",ci" #'lsp-find-implementation))

(use-package flyspell
  :hook (git-commit-setup . flyspell-mode))

(use-package flycheck :straight t
  :init
  (defun my:flycheck-mode ()
    (unless (eq major-mode 'ediff-mode)
      (flycheck-mode)))

  :hook ((go-mode js-mode yaml-mode sh-mode python-mode) . my:flycheck-mode)
  :config
  (evil-define-key* 'normal flycheck-mode-map
    (kbd "]d") #'flycheck-next-error
    (kbd "[d") #'flycheck-previous-error))

(use-package rg :straight t)
(use-package wgrep :straight t)

(use-package dired
  :init
  (defun my:system-open ()
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
    "o" #'my:system-open
    "Z" nil)
  (evil-define-key* 'normal my:intercept-mode-map
    "-" #'dired-jump))

(use-package xclip :straight t
  :config
  (unless
      (display-graphic-p) (xclip-mode +1)))

(use-package undohist :straight t
  :config (undohist-initialize))

(use-package ediff-init :hook ((ediff-quit . delete-frame)))
(use-package vterm :straight t :hook ((vterm-mode . hide-mode-line-mode)))

(use-package go-mode :straight t
  :hook ((go-mode . (lambda nil
                      (setq-local comment-fill-column 150
                                  fill-column 150))))
  :config
  (evil-define-key* 'normal go-mode-map
    "gd" nil ; do not override lsp `go-to-definition'.
    ))

(use-package lua-mode :straight t)

(use-package sh-mode
  :mode "\\.env\\'")

(use-package rainbow-delimiters :straight t
  :hook ((emacs-lisp-mode clojure-mode) . rainbow-delimiters-mode))

(use-package format-all :straight t
  :hook
  ((emacs-lisp-mode clojure-mode go-mode js-mode yaml-mode python-mode) . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

(use-package typescript-mode :straight t)
(use-package yaml-mode :straight t)
(use-package protobuf-mode :straight t)
(use-package dockerfile-mode :straight t)

(use-package markdown-mode :straight t
  :init
  (defun my:markdown-toggle-fontifications (&optional arg)
    "Toggle fontifications on/off."
    (interactive (list (or current-prefix-arg 'toggle)))
    (markdown-toggle-markup-hiding arg))

  :hook (markdown-mode  . my:markdown-toggle-fontifications)
  :config
  (define-key markdown-mode-map (kbd "C-c *") #'my:markdown-toggle-fontifications))

(use-package restclient :straight t :mode ("\\.http\\'" . restclient-mode))
(use-package doom-themes :straight t)
(use-package which-key :straight t :config (which-key-mode 1))
(use-package doom-modeline :straight t :config (doom-modeline-mode 1))

(use-package rainbow-mode :straight t
  :config
  (evil-define-key* 'normal my:intercept-mode-map
    ",tc" #'rainbow-mode))

(use-package dashboard :straight t
  :init
  (add-hook 'after-make-frame-functions
            (lambda (&optional frame)
              (setq initial-buffer-choice (lambda nil
                                            (get-buffer "*dashboard*")))))

  :config (dashboard-setup-startup-hook))

(use-package org
  :init
  (defun my:org-toggle-fontifications ()
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
  (define-key org-mode-map (kbd "C-c *") #'my:org-toggle-fontifications)

  (evil-define-key* 'normal my:intercept-mode-map
    (kbd "SPC ol") #'org-todo-list
    (kbd "SPC ot") #'(lambda nil
                       (interactive)
                       (org-capture nil "n"))))

(use-package org-superstar :straight t)

(use-package isearch
  :init
  (defun my:isearch-region (&rest _)
    "If a region is active, set a selected pattern as an isearch input."
    (interactive "P\np")
    (if mark-active
	    (let ((content (buffer-substring-no-properties (mark) (point))))
		  (deactivate-mark)
		  (isearch-yank-string content))))

  :config
  (advice-add 'isearch-forward :after #'my:isearch-region)
  (advice-add 'isearch-backward :after #'my:isearch-region))

(use-package navigate
  :after evil
  :straight '(navigate
              :type git
              :host github
              :repo "keith/evil-tmux-navigator"))


(use-package elfeed :straight t
  ;; Update elfeed database each 4 hours.
  :config
  (run-with-timer 0 (* 60 60 4) 'elfeed-update)

  (global-set-key(kbd "C-x f") #'elfeed))

(use-package olivetti :straight t :ensure t)
(use-package hide-mode-line :straight t :ensure t)

(use-package zentoggle
  :after (evil olivetti hide-mode-line)
  :hook (elfeed-show-mode . zen-toggle)
  :init
  (evil-define-key* 'normal my:intercept-mode-map
    (kbd ",z") #'zen-toggle))

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

(use-package hl-line
  :hook ((prog-mode org-mode markdown-mode conf-space-mode docker-mode) . hl-line-mode)
  :config
  (evil-define-key* 'normal my:intercept-mode-map
    ",th" #'hl-line-mode))

(use-package flyspell
  :init
  (defun my:flyspell-toggle ()
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

  :config
  (evil-define-key* 'normal my:intercept-mode-map
    ",ts" #'my:flyspell-toggle))

(use-package display-line-numbers
  :hook ((prog-mode org-mode markdown-mode conf-space-mode docker-mode) . display-line-numbers-mode)
  :config
  (evil-define-key* 'normal my:intercept-mode-map
    ",tn" #'display-line-numbers-mode))

(use-package simple
  :init
  (defun my:shutdown-emacs-server ()
    "Quit Emacs globally. Shutdown server."
    (interactive)
    (when (y-or-n-p "Quit emacs and stop the service?")
      (kill-emacs)
      (save-some-buffers)))

  :config
  (evil-define-key* 'normal my:intercept-mode-map
    ",qq" #'my:shutdown-emacs-server
    (kbd "SPC :") #'execute-extended-command
    (kbd "SPC ;") #'eval-expression
    ",to" #'read-only-mode
    ",tn" #'display-line-numbers-mode)

  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (auto-fill-mode 1) ; intelligently breaks content into lines
  )

(use-package emacs
  :init
  (fset 'yes-or-no-p 'y-or-n-p) ; enable y/n answers

  ;; Encoding.
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
  (put 'erase-buffer 'disabled nil))

(use-package hl-todo :straight t
  :config (global-hl-todo-mode))

(use-package prog-mode
  :hook (prog-mode . (lambda ()
                       (setq-local show-trailing-whitespace t))))

(use-package abbrev
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package menu-bar
  :config (menu-bar-mode -1))

(use-package scroll-bar
  :config (scroll-bar-mode -1))

(use-package frame
  :config (blink-cursor-mode -1))

(use-package fringe
  :config (fringe-mode 0))

(use-package delsel
  :config (delete-selection-mode t))

(use-package paren
  :config (show-paren-mode 1))

(use-package autorevert
  :config (global-auto-revert-mode 1))

(use-package elec-pair
  :config (electric-pair-mode 1))

(use-package saveplace
  :config (save-place-mode 1))

(defvar my:theme-file-path "~/.emacs.d/theme"
  "Emacs theme filepath.")

(defvar my:theme-default-name "doom-one-light"
  "Current Emacs theme.")

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun my:save-theme-to-file (path name)
  (with-temp-buffer
    (insert name)
    (write-region (point-min) (point-max) path)))

(defun my:save-current-theme-to-file ()
  (my:save-theme-to-file my:theme-file-path
                         (symbol-name (car custom-enabled-themes))))

(defun my:theme-ensure-exists ()
  (unless (file-exists-p my:theme-file-path)
    (my:save-theme-to-file my:theme-file-path my:theme-default-name)))

(defun my:load-theme-from-file ()
  (my:theme-ensure-exists)
  (load-theme
   (intern
    (string-trim
     (with-temp-buffer
       (insert-file-contents my:theme-file-path)
       (buffer-string))))
   t))

(add-hook 'after-load-theme-hook #'my:save-current-theme-to-file)

(defun my:load-theme-faces (&optional frame)
  "Adjust faces."
  (when frame
    (select-frame frame))
  (my:load-theme-from-file)

  (unless (display-graphic-p)
    ;; Fix terminal vertical-border glyph.
    ;; (https://emacs.stackexchange.com/questions/7228/nice-tty-window-borders-in-24-4).
    (let ((display-table (or standard-display-table (make-display-table))))
      (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; U+2502
      (setq standard-display-table display-table))
    ;; Make a vertical border as a tmux' one.
    (set-face-attribute 'vertical-border frame
                        :foreground (face-foreground 'success)
                        :background (face-background 'default))))

(add-hook 'after-init-hook #'my:load-theme-faces)
(add-hook 'after-make-frame-functions #'my:load-theme-faces)

;; Maximize window on startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(message "Load time %.06f" (float-time (time-since time-emacs-start)))

;;; init.el ends here
