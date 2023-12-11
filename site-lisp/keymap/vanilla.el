(with-eval-after-load 'core/intercept-mode
  ;; As the tmux config uses C-SPS as a prefix key we need to rebind `set-mark-command'.
  (define-key core/intercept-mode-map (kbd "C-q") #'set-mark-command)
  (define-key core/intercept-mode-map (kbd "C-w") #'edit/backward-kill-word-or-region))

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
  (global-set-key [remap switch-to-buffer]                  #'consult-buffer)
  (global-set-key [remap org-agenda]                        #'consult-org-agenda)
  (global-set-key [remap switch-to-buffer-other-window]     #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame]      #'consult-buffer-other-frame)
  (global-set-key [remap yank-pop]                          #'consult-yank-pop))

(with-eval-after-load 'core/project
  (define-key core/intercept-mode-map (kbd "C-x f") #'project-find-file)
  (define-key core/intercept-mode-map (kbd "C-x p") #'project-switch-project)
  (define-key core/intercept-mode-map (kbd "C-x !") #'project-forget-zombie-projects)
  (define-key core/intercept-mode-map (kbd "C-x #") #'project-kill-buffers))

(with-eval-after-load 'edit/core
  (global-set-key (kbd "M-t") #'ace-jump-mode))

(with-eval-after-load 'tools/filesystem
  (global-set-key [remap dired] #'dired-jump))

(provide 'keymap/vanilla)
