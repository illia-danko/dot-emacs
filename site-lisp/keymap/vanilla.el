(with-eval-after-load 'core/core
  (global-set-key (kbd "C-c v") #'imenu)
  (global-set-key (kbd "C-c r") #'recentf))

(with-eval-after-load 'core/project
  (define-key core/intercept-mode-map (kbd "C-x f") #'project-find-file)
  (define-key core/intercept-mode-map (kbd "C-x p") #'project-switch-project)
  (define-key core/intercept-mode-map (kbd "C-x !") #'project-forget-zombie-projects)
  (define-key core/intercept-mode-map (kbd "C-x #") #'project-kill-buffers))

(with-eval-after-load 'completion/core
  (global-set-key (kbd "C-x B") #'consult-project-buffer)
  (global-set-key (kbd "C-c SPC") #'consult-mark)
  (global-set-key (kbd "C-c v") #'consult-imenu)
  (global-set-key (kbd "C-c f") #'completion/consult-ripgrep)
  (global-set-key (kbd "C-c F") #'completion/consult-line-multi)

  (define-key core/intercept-mode-map (kbd "C-.") #'embark-act)
  (define-key core/intercept-mode-map (kbd "C-,") #'embark-dwim))

(with-eval-after-load 'tool/core
  (global-set-key (kbd "C-c b") #'recompile)
  (global-set-key (kbd "C-c B") #'project-compile))

(with-eval-after-load 'tool/filesystem
  (global-set-key [remap dired] #'dired-jump))

(with-eval-after-load 'tool/diff
  (global-set-key (kbd "C-c dw") #'tool/compare-two-open-windows))

(with-eval-after-load 'tool/spelling
  (global-set-key (kbd "C-c s") #'tool/spelling-toggle-buffer)
  (global-set-key (kbd "C-c w") #'ispell-word))

(with-eval-after-load 'edit/core
  ;; As the tmux config uses C-SPS as a prefix key we need to rebind `set-mark-command'.
  (define-key core/intercept-mode-map (kbd "C-q") #'set-mark-command)
  (define-key core/intercept-mode-map (kbd "C-w") #'edit/backward-kill-word-or-region))

(with-eval-after-load 'edit/vanilla
  (define-key core/intercept-mode-map (kbd "C-o") #'er/expand-region)
  (define-key core/intercept-mode-map (kbd "M-o") #'er/contract-region)
  (global-set-key (kbd "C-c m") #'edit/multiple-cursors-keymap/body))

(with-eval-after-load 'tool/version-control
  (global-set-key (kbd "C-x g" ) #'magit-status)
  (global-set-key (kbd "C-c gg") #'magit-status)
  (global-set-key (kbd "C-c gd") #'magit-diff-buffer-file)
  (global-set-key (kbd "C-c gb") #'magit-log-buffer-file)
  (global-set-key (kbd "C-c gL") #'magit-log-all)
  (global-set-key (kbd "C-c ga") #'magit-blame-addition)
  (global-set-key (kbd "C-c gr") #'magit-diff-range) ; show difference between branches
  (global-set-key (kbd "C-c gf") #'magit-find-file) ; visit a file from any branch
  (global-set-key (kbd "C-c gu") #'git-link)
  (global-set-key (kbd "C-c gU") #'tool/browse-project-home-page))

(with-eval-after-load 'text/org
  (define-key org-mode-map (kbd "C-c gc") #'org/git-push-org-file)
  (define-key org-mode-map (kbd "<C-tab>") #'org-fold-show-all)
  (global-set-key (kbd "C-c oa") #'org-agenda)
  (global-set-key (kbd "C-c ot") #'org/capture-todo)
  (global-set-key (kbd "C-c od") #'org/capture-diary)
  (global-set-key (kbd "C-c 4") #'org/toggle-fontifications)
  (global-set-key (kbd "C-c of") #'org/consult-ripgrep))

(with-eval-after-load 'ui/font
  (global-set-key (kbd "C-+") #'text-scale-increase)
  (global-set-key (kbd "C-\)") #'text-scale-decrease))

(with-eval-after-load 'ui/rgb-highlight
  (global-set-key (kbd "C-c ^") #'rainbow-mode))

(provide 'keymap/vanilla)
