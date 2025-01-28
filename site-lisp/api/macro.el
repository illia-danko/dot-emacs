(defmacro api/customize-set-variable* (&rest body)
  (let ((vars (seq-map #'(lambda (x)
						   (cons 'customize-set-variable x))
					   (seq-split body 2))))
	`(progn
	   ,@vars
	   )))

(defmacro defun-iteractive(name body)
  `(defun ,name (&optional arg)
     (interactive)
	 (progn
	   ,body)))

(provide 'api/macro)
