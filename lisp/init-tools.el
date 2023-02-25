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
  :bind
  ("C-x f" . elfeed)
  :custom
  (elfeed-search-filter "@6-months-ago +unread") ; keep last 6 mounth.
  :config
  (run-with-timer 0 (* 60 60 4) #'elfeed-update)) ; update elfeed database every 4 hours.

;; Test rest api from Emacs.
(use-package restclient :straight t
  :mode ("\\.http\\'" . restclient-mode))

(provide 'init-tools)
