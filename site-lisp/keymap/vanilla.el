(require 'core/core)
(require 'core/project)
(require 'completion/core)
(require 'completion/lsp)
(require 'tool/core)
(require 'tool/diff)
(require 'tool/spelling)
(require 'tool/version-control)
(require 'edit/core)
(require 'edit/vanilla)
(require 'text/org)
(require 'text/markdown)
(require 'lang/emacs-lisp)
(require 'lang/elixir)
(require 'ui/font)
(require 'ui/rgb-highlight)
(require 'ui/zen)

(global-set-key (kbd "C-c v") #'imenu)
(global-set-key (kbd "C-c r") #'recentf)
(global-set-key (kbd "C-c on") #'display-line-numbers-mode)
(global-set-key (kbd "C-c ow") #'core/toggle-highlight-whitespaces)

(define-key core/intercept-mode-map (kbd "C-c f") #'project-find-file)
(define-key core/intercept-mode-map (kbd "C-x p") #'project-switch-project)
(define-key core/intercept-mode-map (kbd "C-x !") #'project-forget-zombie-projects)
(define-key core/intercept-mode-map (kbd "C-x #") #'project-kill-buffers)

(global-set-key (kbd "C-x B") #'consult-project-buffer)
(global-set-key (kbd "C-c f") #'completion/consult-ripgrep)
(global-set-key (kbd "C-c F") #'completion/consult-line-multi)

(define-key core/intercept-mode-map (kbd "C-.") #'embark-act)
(define-key core/intercept-mode-map (kbd "C-,") #'embark-dwim)

(define-key eglot-mode-map (kbd "C-c cr") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c ci") #'eglot-find-implementation)

(global-set-key (kbd "C-c b") #'recompile)
(global-set-key (kbd "C-c B") #'project-compile)
(define-key core/intercept-mode-map (kbd "C-q") #'set-mark-command)

(global-set-key (kbd "C-c dw") #'tool/compare-two-open-windows)

(global-set-key (kbd "C-c s") #'tool/spelling-toggle-buffer)
(global-set-key (kbd "C-c w") #'ispell-word)

(global-set-key (kbd "C-x g" ) #'magit-status)
(global-set-key (kbd "C-c gg") #'magit-status)
(global-set-key (kbd "C-c gd") #'magit-diff-buffer-file)
(global-set-key (kbd "C-c gb") #'magit-log-buffer-file)
(global-set-key (kbd "C-c gL") #'magit-log-all)
(global-set-key (kbd "C-c ga") #'magit-blame-addition)
(global-set-key (kbd "C-c gr") #'magit-diff-range) ; show difference between branches
(global-set-key (kbd "C-c gf") #'magit-find-file) ; visit a file from any branch
(global-set-key (kbd "C-c gu") #'git-link)
(global-set-key (kbd "C-c gU") #'tool/browse-project-home-page)
(global-set-key (kbd "C-c n") #'git-gutter:next-hunk)
(global-set-key (kbd "C-c p") #'git-gutter:previous-hunk)
(global-set-key (kbd "C-c n") #'git-gutter:previous-hunk)
(global-set-key (kbd "C-c hu") #'git-gutter:revert-hunk)
(global-set-key (kbd "C-c hp") #'tool/vc-git-gutter-popup-hunk-jump)

(define-key core/intercept-mode-map (kbd "C-t") #'er/expand-region)
(define-key core/intercept-mode-map (kbd "M-t") #'er/contract-region)

(global-set-key (kbd "C-c m") #'edit/multiple-cursors-keymap/body)

(define-key org-mode-map (kbd "C-c gc") #'text/org-git-push-org-file)
(define-key org-mode-map (kbd "<C-tab>") #'org-fold-show-all)
(define-key org-mode-map (kbd "C-c 4") #'text/org-toggle-fontifications)
(global-set-key (kbd "C-c oa") #'org-agenda)
(global-set-key (kbd "C-c ot") #'text/org-capture-todo)
(global-set-key (kbd "C-c od") #'text/org-capture-diary)
(global-set-key (kbd "C-c of") #'text/org-consult-ripgrep)

(define-key markdown-mode-map (kbd "<tab>") #'outline-toggle-children)
(define-key markdown-mode-map (kbd "C-c 4") #'text/markdown-toggle-fontifications)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'eval-defun)

(define-key elixir-ts-mode-map (kbd "C-c or") #'inf-elixir-project) ; alias to repl
(define-key elixir-ts-mode-map (kbd "C-c C-b") #'inf-elixir-send-buffer)
(define-key elixir-ts-mode-map (kbd "C-c C-c") #'lang/elixir-eval-line-or-region)

(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C-\)") #'text-scale-decrease)

(global-set-key (kbd "C-c ^") #'rainbow-mode)

(global-set-key (kbd "C-c z") #'ui/zen-toggle)

(provide 'keymap/vanilla)
