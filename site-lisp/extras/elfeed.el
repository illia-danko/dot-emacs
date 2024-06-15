(require 'elfeed)
(require 'api/macro)

(api/customize-set-variable* 'elfeed-search-filter "@6-months-ago +unread") ; keep last 6 mounth.

;; Autotagging.
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "youtube\\.com"
													 :add '(video youtube)))
;; Remove old entries.
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 weeks ago"
													 :remove 'unread))

;; Set buffer's text width to match the content.
(advice-add 'elfeed-show-entry :after #'(lambda (&rest _) (setq-local fill-column 120)))

(provide 'extras/elfeed)
