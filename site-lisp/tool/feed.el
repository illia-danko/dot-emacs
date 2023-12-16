(progn
  (with-eval-after-load 'elfeed
	(customize-set-variable 'elfeed-search-filter "@6-months-ago +unread") ; keep last 6 mounth.

	;; Autotagging.
	(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "youtube\\.com"
														 :add '(video youtube)))
	;; Remove old entries.
	(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 weeks ago"
														 :remove 'unread)))
  (require 'elfeed))

(provide 'tool/feed)
