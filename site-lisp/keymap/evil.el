(require 'edit/evil)
(require 'core/intercept-mode)
(require 'core/project)
(require 'tool/vterm-evil)
(require 'tool/dired)

;; Core.
(global-set-key [remap evil-copy-from-above] #'yank)

(evil-define-key* '(normal) vterm-copy-mode-map
  (kbd "i") #'tool/vterm-evil-exit-copy-mode
  (kbd "I") #'tool/vterm-evil-exit-copy-mode
  (kbd "a") #'tool/vterm-evil-exit-copy-mode
  (kbd "A") #'tool/vterm-evil-exit-copy-mode
  (kbd "o") #'tool/vterm-evil-exit-copy-mode
  (kbd "O") #'tool/vterm-evil-exit-copy-mode)

(evil-define-key* '(normal) dired-mode-map
  (kbd "O") #'tool/dired-system-open)

(require 'tool/gutter)
(evil-define-key* '(normal) global-map
  (kbd "]c") #'git-gutter:next-hunk
  (kbd "[c") #'git-gutter:previous-hunk
  (kbd "SPC hu") #'git-gutter:revert-hunk
  (kbd "SPC hp") #'tool/git-gutter-popup-hunk-jump)

(require 'tool/version-control)
(evil-define-key* '(normal) global-map
  (kbd ", gg") #'magit-status
  (kbd ", gd") #'magit-diff-buffer-file
  (kbd ", gb") #'magit-log-buffer-file
  (kbd ", gL") #'magit-log-all
  (kbd ", ga") #'magit-blame-addition
  (kbd ", gr") #'magit-diff-range
  (kbd ", gf") #'magit-find-file
  (kbd ", gu") #'git-link
  (kbd ", gU") #'tool/browse-project-home-page
  (kbd ", gw") #'tool/compare-two-open-windows)

(require 'completion/lsp)
(evil-define-key* '(normal) global-map
  (kbd "gi") #'eglot-find-implementation
  (kbd "gn") #'eglot-rename)

(evil-define-key* '(normal) core/intercept-mode-map
  (kbd "C-t") #'project-find-file
  (kbd ",b") #'switch-to-buffer)

(provide 'keymap/evil)
