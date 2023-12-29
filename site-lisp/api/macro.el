(defmacro api/customize-set-variable* (&rest body)
  (let ((vars (seq-map #'(lambda (x)
						   (cons 'customize-set-variable x))
					   (seq-split body 2))))
	`(progn
	   ,@vars
	   )))

(provide 'api/macro)
