(require 'edit/evil)
(require 'core/intercept-mode)
(require 'core/project)
(require 'tool/vterm-evil)
(require 'tool/dired)
(require 'tool/gutter)
(require 'completion/lsp)

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

(evil-define-key* '(normal) global-map
				  (kbd "]c") #'git-gutter:next-hunk
				  (kbd "[c") #'git-gutter:previous-hunk
				  (kbd "SPC hu") #'git-gutter:revert-hunk
				  (kbd "SPC hp") #'tool/git-gutter-popup-hunk-jump)

(evil-define-key* '(normal) global-map
				  (kbd "gi") #'eglot-find-implementation
				  (kbd "gn") #'eglot-rename)

(evil-define-key* '(normal) core/intercept-mode-map
				  (kbd "C-t") #'project-find-file
				  (kbd ",b") #'switch-to-buffer)

(provide 'keymap/evil)
