(use-package elfeed :straight t
  :bind
  ("C-x f" . elfeed)
  :custom
  (elfeed-search-filter "@6-months-ago +unread") ; keep last 6 mounth.
  ;; Update elfeed database each 4 hours.
  :config
  (run-with-timer 0 (* 60 60 4) #'elfeed-update))

(provide 'init-elfeed)
