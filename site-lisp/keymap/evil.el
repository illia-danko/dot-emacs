(require 'edit/evil)
(require 'core/intercept-mode)
(require 'core/project)
(require 'tool/vterm-evil)
(require 'text/org-evil)
(require 'tool/dired)
(require 'tool/gutter)
(require 'completion/lsp)

;; Core.
(global-set-key [remap evil-copy-from-above] #'yank)
(define-key evil-normal-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-motion-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-insert-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-window-map         (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-operator-state-map (kbd "C-g") #'edit/evil-keyboard-quit)

(evil-define-key* '(normal) vterm-copy-mode-map
  (kbd "i") #'tool/vterm-evil-exit-copy-mode
  (kbd "I") #'tool/vterm-evil-exit-copy-mode
  (kbd "a") #'tool/vterm-evil-exit-copy-mode
  (kbd "A") #'tool/vterm-evil-exit-copy-mode
  (kbd "o") #'tool/vterm-evil-exit-copy-mode
  (kbd "O") #'tool/vterm-evil-exit-copy-mode)

(evil-define-key* '(normal) dired-mode-map
  (kbd "O") #'tool/dired-system-open)

(evil-define-key* '(normal) org-mode-map
  (kbd "RET") #'org-open-at-point)

(evil-define-key* '(normal) global-map
  (kbd "]c") #'git-gutter:next-hunk
  (kbd "[c") #'git-gutter:previous-hunk
  (kbd "SPC hu") #'git-gutter:revert-hunk
  (kbd "SPC hp") #'tool/git-gutter-popup-hunk-jump)

(evil-define-key* '(normal) global-map
  (kbd "gi") #'eglot-find-implementation
  (kbd "gn") #'eglot-rename)

(evil-define-key* '(normal) core/intercept-mode-map
  (kbd "C-t") #'project-find-file)

(provide 'keymap/evil)
