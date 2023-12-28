(progn
  (with-eval-after-load 'epg
	(advice-add 'epg-wait-for-status :around #'(lambda (orig-fun &rest args)))))

(provide 'tool/gpg)
