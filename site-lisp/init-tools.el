;; Required by `rg'.
(use-package wgrep :straight t
  :custom
  (wgrep-auto-save-buffer t) ; automatically save buffers after edit
  )

;; Bulk search/replace.
(use-package rg :straight t
  :after (wgrep))

;; Read rss.
(use-package elfeed :straight t
  :custom
  (elfeed-search-filter "@6-months-ago +unread") ; keep last 6 mounth.
  :config
  (run-with-timer 0 (* 60 60 4) #'elfeed-update) ; update elfeed database every 4 hours.
  :bind
  ("C-x w" . elfeed))

;; Test rest api from Emacs.
(use-package restclient :straight t
  :mode ("\\.http\\'" . restclient-mode))

;; Emacs startup greeter.
(use-package dashboard :straight t
  :init
  ;; Open dashboard when frame created.
  (add-hook 'after-make-frame-functions
            (lambda (&optional frame)
              (setq initial-buffer-choice (lambda nil
                                            (get-buffer "*dashboard*")))))

  :custom
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda) ; show todo entries
  (dashboard-items '((agenda . 8) (projects . 4) (recents . 4))) ; layout
  (dashboard-projects-backend 'project-el) ; use project-el as project backend
  (dashboard-set-footer nil) ; do not display footer

  :config
  (dashboard-setup-startup-hook))

(use-package leetcode :straight t
  :ensure t
  :custom
  (leetcode-prefer-language "golang")
  (leetcode-directory "~/github.com/illia-danko/emacs-leetcode")
  (leetcode-save-solutions t))

(use-package olivetti :straight t
  :bind
  ("C-c z" . olivetti-mode)
  :hook
  (elfeed-show-mode . olivetti-mode))

(use-package hide-mode-line :straight t)

;; Kubernetes frontend.
(use-package kubel :straight t)

(provide 'init-tools)
