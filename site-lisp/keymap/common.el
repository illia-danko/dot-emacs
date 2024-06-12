(require 'emacs)
(require 'core/intercept-mode)
(require 'core/project)
(require 'completion/core)
(require 'completion/minibuffer)
(require 'tool/core)
(require 'tool/version-control)
(require 'tool/dired)
(require 'tool/vterm)
(require 'tool/spelling)
(require 'tool/gutter)
(require 'edit/core)
(require 'text/org)
(require 'text/org-roam)
(require 'lang/elixir)

(global-set-key [remap kill-buffer] #'kill-this-buffer)

(global-set-key (kbd "C-c v") #'imenu)
(global-set-key (kbd "C-c r") #'recentf)

(global-set-key (kbd "C-c wl") #'display-line-numbers-mode)
(global-set-key (kbd "C-c ww") #'core/toggle-display-whitespaces)

(define-key eglot-mode-map (kbd "C-c cr") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c ci") #'eglot-find-implementation)

(global-set-key [remap next-matching-history-element]     #'consult-history)
(global-set-key [remap previous-matching-history-element] #'consult-history) ; M-r in minibuffer-local-map
(global-set-key [remap apropos]                           #'consult-apropos)
(global-set-key [remap bookmark-jump]                     #'consult-bookmark)
(global-set-key [remap goto-line]                         #'consult-goto-line)
(global-set-key [remap locate]                            #'consult-locate)
(global-set-key [remap load-theme]                        #'consult-theme)
(global-set-key [remap man]                               #'consult-man)
(global-set-key [remap recentf-open-files]                #'consult-recent-file)
(global-set-key [remap switch-to-buffer]                  #'consult-project-buffer)
(global-set-key [remap org-agenda]                        #'consult-org-agenda)
(global-set-key [remap switch-to-buffer-other-window]     #'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame]      #'consult-buffer-other-frame)
(global-set-key [remap yank-pop]                          #'consult-yank-pop)
(global-set-key [remap switch-to-buffer]                  #'consult-buffer)
(global-set-key [remap imenu]                             #'consult-imenu)

(define-key core/intercept-mode-map (kbd "C-x pz") #'project-forget-zombie-projects)

;; Uses in conjunction with `project-switch-commands'.
(define-key project-prefix-map "t" #'vterm)
(define-key project-prefix-map "g" #'tool/magit-status)
(define-key project-prefix-map "R" #'rg)
(define-key project-prefix-map "S" #'project-find-regexp)
(define-key project-prefix-map "B" #'project-compile)

(define-key core/intercept-mode-map (kbd "C-c gg") #'magit-status)
(define-key core/intercept-mode-map (kbd "C-c gd") #'magit-diff-buffer-file)
(define-key core/intercept-mode-map (kbd "C-c gb") #'magit-log-buffer-file)
(define-key core/intercept-mode-map (kbd "C-c gL") #'magit-log-all)
(define-key core/intercept-mode-map (kbd "C-c ga") #'magit-blame-addition)
(define-key core/intercept-mode-map (kbd "C-c gr") #'magit-diff-range) ; show difference between branches
(define-key core/intercept-mode-map (kbd "C-c gf") #'magit-find-file) ; visit a file from any branch
(define-key core/intercept-mode-map (kbd "C-c gu") #'git-link)
(define-key core/intercept-mode-map (kbd "C-c gU") #'tool/browse-project-home-page)
(define-key core/intercept-mode-map (kbd "C-c gw") #'tool/compare-two-open-windows)
(define-key core/intercept-mode-map (kbd "C-c n") #'git-gutter:next-hunk)
(define-key core/intercept-mode-map (kbd "C-c p") #'git-gutter:previous-hunk)
(define-key core/intercept-mode-map (kbd "C-c hu") #'git-gutter:revert-hunk)
(define-key core/intercept-mode-map (kbd "C-c hp") #'tool/git-gutter-popup-hunk-jump)

(define-key core/intercept-mode-map (kbd "C-x B") #'consult-project-buffer)
(define-key core/intercept-mode-map (kbd "C-c s") #'completion/consult-ripgrep)
(define-key core/intercept-mode-map (kbd "C-c S") #'completion/consult-line-multi)

(define-key core/intercept-mode-map (kbd "C-.") #'embark-act)
(define-key core/intercept-mode-map (kbd "C-,") #'embark-dwim)

(define-key vertico-map (kbd "M-r") #'vertico-exit-input)

(define-key core/intercept-mode-map (kbd "C-c b") #'recompile)
(define-key core/intercept-mode-map (kbd "C-c B") #'project-compile)

(global-set-key [remap dired] #'dired-jump)
(define-key dired-mode-map "O" #'tool/dired-system-open)

(global-set-key (kbd "C-c t") #'tool/vterm-project)
(global-set-key (kbd "C-c T") #'vterm)

(define-key core/intercept-mode-map (kbd "C-c !") #'tool/spelling-toggle-buffer)
(define-key core/intercept-mode-map (kbd "C-M-S-i") #'ispell-word)

(global-set-key (kbd "M-j") #'join-line)
(global-set-key (kbd "C-w") #'edit/backward-kill-word-or-region)
(global-set-key (kbd "C-c SPC") #'ace-jump-mode)
(global-set-key (kbd "M-'") #'anzu-query-replace)
(global-set-key (kbd "M-\"") #'anzu-query-replace-regexp)

(define-key core/intercept-mode-map (kbd "C-o") #'er/expand-region)
(define-key core/intercept-mode-map (kbd "M-o") #'er/contract-region)
(global-set-key (kbd "C-c m") #'edit/multiple-cursors-keymap/body)

(define-key markdown-mode-map (kbd "<C-tab>") #'outline-toggle-children)
(define-key markdown-mode-map (kbd "C-c 4") #'text/markdown-toggle-fontifications)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-b") #'eval-buffer)

(define-key elixir-ts-mode-map (kbd "C-c cp") #'inf-elixir-project) ; alias to repl
(define-key elixir-ts-mode-map (kbd "C-c cP") #'lang/inf-elixir-phoenix-server)

(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C-\)") #'text-scale-decrease)
(global-set-key (kbd "C-c ^") #'rainbow-mode)
(global-set-key (kbd "C-c z") #'ui/zen-toggle)

;; Org.
(define-key org-mode-map (kbd "C-c gc") #'text/org-git-push-org-file)
(define-key org-mode-map (kbd "<C-tab>") #'org-cycle)
(define-key org-mode-map (kbd "C-c 4") #'text/org-toggle-fontifications)
(define-key core/intercept-mode-map (kbd "C-c oa") #'org-agenda)
(define-key core/intercept-mode-map (kbd "C-c ot") #'text/org-capture-todo)
(define-key core/intercept-mode-map (kbd "C-c od") #'text/org-capture-diary)
(define-key core/intercept-mode-map (kbd "C-c ob") #'text/org-capture-slipbox)

;; Org Roam.
(define-key core/intercept-mode-map (kbd "C-c ol") #'org-roam-backlinks-and-links)
(define-key core/intercept-mode-map (kbd "C-c of") #'org-roam-node-find)
(define-key core/intercept-mode-map (kbd "C-c og") #'org-roam-graph)
(define-key core/intercept-mode-map (kbd "C-c oi") #'org-roam-node-insert)
(define-key core/intercept-mode-map (kbd "C-c oc") #'org-roam-capture)
(define-key core/intercept-mode-map (kbd "C-c oC") #'org-roam-dailies-capture-today)
(define-key core/intercept-mode-map (kbd "C-c os") #'text/org-roam-consult-ripgrep)

;; org-download.
(define-key org-mode-map (kbd "C-c i") #'org-download-clipboard)

(provide 'keymap/common)
