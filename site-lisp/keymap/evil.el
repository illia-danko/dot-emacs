;;;  evil.el --- Keybinding for vim mode  -*- lexical-binding:t; -*-

(require 'edit/evil)
(require 'core/intercept-mode)

;; Core.
(evil-define-key* '(normal) global-map
  (kbd ", d#") #'server-force-delete)

(require 'recentf)
(evil-define-key* '(normal) global-map
  (kbd ", r") #'recentf)

;; Bookmarks and shortcuts.
(require 'edit/buffers)
(require 'dashboard)
(evil-define-key* '(normal) global-map
  (kbd "g md") #'dashboard-open
  (kbd "g ma") #'bookmark-jump
  (kbd "g mn") #'bookmark-set
  (kbd "g ms") #'edit/switch-to-scratch-buffer
  (kbd "g mm") #'edit/switch-to-messages-buffer
  (kbd "g mr") #'bookmark-delete)

;; Completion.
(evil-define-key* '(insert) global-map
  (kbd "C-t") #'complete-symbol)

(require 'corfu)
(evil-define-key* '(insert) corfu-map
  (kbd "C-t") #'corfu-complete)

;; Optional: Make C-y work in insert and visual states too
(evil-define-key* '(visual replace insert normal) global-map
  (kbd "C-y") #'consult-yank-pop)

(require 'tempel)
(evil-define-key* '(insert) tempel-map
  (kbd "C-f") #'tempel-next
  (kbd "C-b") #'tempel-previous
  (kbd "C-q") #'tempel-abort)

;; Rest.
(require 'tool/vterm-evil)
(evil-define-key* '(normal) vterm-copy-mode-map
  (kbd "i") #'tool/vterm-evil-exit-copy-mode
  (kbd "I") #'tool/vterm-evil-exit-copy-mode
  (kbd "a") #'tool/vterm-evil-exit-copy-mode
  (kbd "A") #'tool/vterm-evil-exit-copy-mode
  (kbd "o") #'tool/vterm-evil-exit-copy-mode
  (kbd "O") #'tool/vterm-evil-exit-copy-mode
  (kbd "<escape>") #'switch-to-prev-buffer
  (kbd "C-g") #'switch-to-prev-buffer)

(evil-define-key* '(emacs) vterm-mode-map
  (kbd "<escape>") #'tool/vterm-evil-copy-mode-normal-mode
  (kbd "C-g") #'tool/vterm-evil-copy-mode-normal-mode)

(require 'tool/dired)
(evil-define-key* '(normal) dired-mode-map
  (kbd "O") #'tool/dired-system-open)

(require 'tool/gutter)
(evil-define-key* '(normal) global-map
  (kbd "]c") #'git-gutter:next-hunk
  (kbd "[c") #'git-gutter:previous-hunk
  (kbd "ghu") #'git-gutter:revert-hunk
  (kbd "ghp") #'tool/git-gutter-popup-hunk-jump)

(require 'tool/version-control)
(evil-define-key* '(normal) global-map
  (kbd ", gg") #'magit-status
  (kbd ", gs") #'magit-status
  (kbd ", gd") #'magit-diff-buffer-file
  (kbd ", gb") #'magit-log-buffer-file
  (kbd ", gl") #'magit-log-all
  (kbd ", ga") #'magit-blame-addition
  (kbd ", gp") #'magit-diff-range
  (kbd ", gf") #'magit-find-file
  (kbd ", gu") #'git-link
  (kbd ", gU") #'tool/browse-project-home-page
  (kbd ", gw") #'tool/compare-two-open-windows)

(require 'completion/lsp)
(evil-define-key* '(normal) eglot-mode-map
  (kbd "gi") #'eglot-find-implementation
  (kbd "gn") #'eglot-rename)

(require 'completion/core)
(require 'core/project)
(require 'consult)
(evil-define-key* '(normal) core/intercept-mode-map
  (kbd "C-q") #'project-switch-project
  (kbd "C-t") #'project-find-file
  (kbd ", b") #'switch-to-buffer
  (kbd ", B") #'consult-project-buffer)

(evil-define-key* '(normal visual) core/intercept-mode-map
  (kbd ", ss") #'completion/consult-ripgrep
  (kbd ", sS") #'completion/consult-line-multi)

(require 'tool/vterm-evil)
(evil-define-key* '(normal) global-map
  (kbd ", tt") #'tool/vterm-project
  (kbd ", tT") #'vterm)

(require 'tool/spelling)
(evil-define-key* '(normal) global-map
  (kbd ", se") #'tool/spelling-toggle-buffer)

(require 'core/project)
(evil-define-key* '(normal) global-map
  (kbd ", db") #'kill-this-buffer
  (kbd ", dp") #'(lambda () (interactive) (project-kill-buffers t)))

(require 'imenu)
(evil-define-key* '(normal) global-map
  (kbd ", v") #'imenu)

(require 'ui/zen)
(evil-define-key* '(normal) global-map
  (kbd ", wz") #'ui/zen-toggle)

(require 'ace-jump-mode)
(evil-define-key* '(normal) global-map
  (kbd "M-t") #'ace-jump-mode)

(require 'hydra)
(require 'expand-region)
(defhydra edit/expand-region (:color blue)
  "Expand Region"
  ("r" er/expand-region "expand-region" :exit nil)
  ("R" er/contract-region "contract-region" :exit nil)
  ("q" nil "cancel"))

(evil-define-key* '(normal) global-map
  (kbd "g e") #'edit/expand-region/body)

(require 'rainbow-mode)
(evil-define-key* '(normal) global-map
  (kbd ", wr") #'rainbow-mode)

(provide 'keymap/evil)

;;; evil.el ends here
