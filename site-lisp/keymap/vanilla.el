(with-eval-after-load 'consult
  (global-set-key [remap next-matching-history-element] #'consult-history)
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

(with-eval-after-load 'project
  (define-key core/intercept-mode-map (kbd "C-x f") #'project-find-file)
  (define-key core/intercept-mode-map (kbd "C-x p") #'project-switch-project)
  (define-key core/intercept-mode-map (kbd "C-x !") #'project-forget-zombie-projects)
  (define-key core/intercept-mode-map (kbd "C-x #") #'project-kill-buffers))

(require 'project)

(provide 'keymap/vanilla)
