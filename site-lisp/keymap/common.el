(require 'emacs)
(require 'core/intercept-mode)
(require 'completion/core)
(require 'completion/minibuffer)
(require 'tool/core)
(require 'tool/filesystem)
(require 'tool/vterm)
(require 'tool/spelling)
(require 'edit/core)

(global-set-key [remap kill-buffer] #'kill-this-buffer)

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

(global-set-key (kbd "C-c e") #'tool/vterm-project)
(global-set-key (kbd "C-c E") #'vterm)

(define-key core/intercept-mode-map (kbd "C-c os") #'tool/spelling-toggle-buffer)
(define-key core/intercept-mode-map (kbd "C-c w") #'ispell-word)


(define-key core/intercept-mode-map (kbd "C-w") #'edit/backward-kill-word-or-region)
(global-set-key (kbd "C-c SPC") #'ace-jump-mode)
(global-set-key [remap query-replace] #'anzu-query-replace)
(global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)

(provide 'keymap/common)
