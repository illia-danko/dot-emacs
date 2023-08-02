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
  )

;; Test rest api from Emacs.
(use-package restclient :straight t
  :mode ("\\.http\\'" . restclient-mode))

;; Train your brain with code challenges.
(use-package leetcode :straight t
  :ensure t
  :custom
  (leetcode-prefer-language "golang")
  (leetcode-directory "~/github.com/illia-danko/emacs-leetcode")
  (leetcode-save-solutions t))

;; Center screen content.
(use-package olivetti :straight t
  :bind
  ("C-c z" . olivetti-mode)
  :hook
  (elfeed-show-mode . olivetti-mode))

;; Do not show modeline.
(use-package hide-mode-line :straight t)

;; Highlight RGB colors.
(use-package rainbow-mode :straight t
  :bind (("C-c ^" . rainbow-mode)))

;; Kubernetes frontend.
(use-package kubel :straight t)

(provide 'init-tools)
