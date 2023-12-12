(with-eval-after-load 'core/project
  (define-key core/intercept-mode-map (kbd "C-x f") #'project-find-file)
  (define-key core/intercept-mode-map (kbd "C-x p") #'project-switch-project)
  (define-key core/intercept-mode-map (kbd "C-x !") #'project-forget-zombie-projects)
  (define-key core/intercept-mode-map (kbd "C-x #") #'project-kill-buffers))

(with-eval-after-load 'completion/core
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
  (global-set-key [remap switch-to-buffer]                  #'consult-project-buffer)
  (global-set-key (kbd "C-x B")                             #'consult-buffer)
  (global-set-key (kbd "C-c SPC")                           #'consult-mark)
  (global-set-key (kbd "C-c v")                             #'consult-imenu)
  (global-set-key (kbd "C-c f")                             #'completion/consult-ripgrep)
  (global-set-key (kbd "C-c F")                             #'completion/consult-line-multi)

  (with-eval-after-load 'tool/core
	(global-set-key (kbd "C-c b") #'recompile)
	(global-set-key (kbd "C-c B") #'project-compile))

  (with-eval-after-load 'tool/filesystem
	(global-set-key [remap dired] #'dired-jump))

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
	(global-set-key (kbd "C-c gc") #'org/git-push-org-file))

  (provide 'keymap/vanilla)
