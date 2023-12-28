(require 'epg)

;; Monkey patch `epg-wait-for-status' to avoid emacs freeze.
(advice-add 'epg-wait-for-status :around #'(lambda (orig-fun &rest args)))

(provide 'tool/gpg)
