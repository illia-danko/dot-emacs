(require 'core/intercept-mode)

(with-eval-after-load 'core/core
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd ",v") #'imenu
	(kbd ",r") #'recentf
	(kbd "SPC on") #'display-line-numbers-mode
	(kbd "SPC ow") #'core/toggle-highlight-whitespaces))

(with-eval-after-load 'core/project
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-c f") #'project-find-file
	(kbd "C-x p") #'project-switch-project
	(kbd "C-x !") #'project-forget-zombie-projects
	(kbd "C-x #") #'project-kill-buffers))

(with-eval-after-load 'completion/core
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-x B") #'consult-project-buffer
	(kbd "SPC /") #'completion/consult-ripgrep
	(kbd ", s") #'completion/consult-line-multi)

  (evil-define-key* '(visual) core/intercept-mode-map
	(kbd "SPC /") #'completion/consult-ripgrep
	(kbd ", s") #'completion/consult-line-multi)

  (evil-define-key* nil core/intercept-mode-map
	(kbd "C-.") #'embark-act
	(kbd "C-,") #'embark-dwim))

(with-eval-after-load 'completion/lsp
  (evil-define-key* '(normal) eglot-mode-map
	(kbd "gr") #'eglot-rename
	(kbd "gi") #'eglot-find-implementation))

(with-eval-after-load 'tool/diff
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-c dw") #'tool/compare-two-open-windows))

(with-eval-after-load 'tool/spelling
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-c s") #'tool/spelling-toggle-buffer
	(kbd "C-c w") #'ispell-word))

(with-eval-after-load 'tool/vterm-evil
  (evil-define-key* '(normal) vterm-copy-mode-map
	(kbd "i") #'tool/vterm-evil-exit-copy-mode
	(kbd "I") #'tool/vterm-evil-exit-copy-mode
	(kbd "a") #'tool/vterm-evil-exit-copy-mode
	(kbd "A") #'tool/vterm-evil-exit-copy-mode
	(kbd "o") #'tool/vterm-evil-exit-copy-mode
	(kbd "O") #'tool/vterm-evil-exit-copy-mode))

(with-eval-after-load 'edit/core
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-t") #'er/expand-region
	(kbd "M-t") #'er/contract-region))

(with-eval-after-load 'tool/version-control
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-x g" ) #'magit-status
	(kbd ", gg") #'magit-status
	(kbd ", gd") #'magit-diff-buffer-file
	(kbd ", gb") #'magit-log-buffer-file
	(kbd ", gL") #'magit-log-all
	(kbd ", ga") #'magit-blame-addition
	(kbd ", gr") #'magit-diff-range ; show difference between branches
	(kbd ", gf") #'magit-find-file ; visit a file from any branch
	(kbd ", gu") #'git-link
	(kbd ", gU") #'tool/browse-project-home-page
	(kbd "]c") #'git-gutter:next-hunk
	(kbd "[c") #'git-gutter:previous-hunk
	(kbd "SPC hu") #'git-gutter:revert-hunk
	(kbd "SPC hp") #'tool/vc-git-gutter-popup-hunk-jump)
  )

(with-eval-after-load 'text/org
  (evil-define-key* '(normal) org-mode-map
	(kbd ", gc") #'text/org-git-push-org-file
	(kbd "<C-tab>") #'org-fold-show-all
	(kbd "C-c 4") #'text/org-toggle-fontifications)

  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "SPC oa") #'org-agenda
	(kbd "SPC ot") #'text/org-capture-todo
	(kbd "SPC od") #'text/org-capture-diary
	(kbd "SPC of") #'text/org-consult-ripgrep))

(with-eval-after-load 'text/markdown
  (evil-define-key* '(normal) markdown-mode-map
	(kbd "<tab>") #'outline-toggle-children
	(kbd "C-c 4") #'text/markdown-toggle-fontifications))

(with-eval-after-load 'lang/emacs-lisp
  (evil-define-key* '(normal) emacs-lisp-mode-map
	(kbd ", ee") #'eval-defun
	(kbd ", eb") #'eval-buffer)
  (evil-define-key* '(normal) lisp-interaction-mode-map
	(kbd ", ee") #'eval-defun
	(kbd ", eb") #'eval-buffer)
  (evil-define-key* nil emacs-lisp-mode-map
	(kbd "C-c C-c") #'eval-defun
	(kbd "C-c C-b") #'eval-buffer)
  (evil-define-key* nil lisp-interaction-mode-map
	(kbd "C-c C-c") #'eval-defun
	(kbd "C-c C-b") #'eval-buffer))

(with-eval-after-load 'lang/elixir
  (evil-define-key* '(normal) elixir-ts-mode-map
	(kbd "C-c or") #'inf-elixir-project ; alias to repl
	(kbd ", eb") #'inf-elixir-send-buffer
	(kbd ", ee") #'lang/elixir-eval-line-or-region))

(with-eval-after-load 'ui/font
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-+") #'text-scale-increase
	(kbd "C-\)") #'text-scale-decrease))

(with-eval-after-load 'ui/rgb-highlight
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd "C-c ^") #'rainbow-mode))

(with-eval-after-load 'ui/zen
  (evil-define-key* '(normal) core/intercept-mode-map
	(kbd ",z") #'ui/zen-toggle))

;; Other settings.

(with-eval-after-load 'edit/evil
  (define-key evil-normal-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
  (define-key evil-motion-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
  (define-key evil-insert-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
  (define-key evil-window-map         (kbd "C-g") #'edit/evil-keyboard-quit)
  (define-key evil-operator-state-map (kbd "C-g") #'edit/evil-keyboard-quit)
  )

(provide 'keymap/evil)
