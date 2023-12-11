(defun api/upsert-car-string (list item)
  "Insert or update an item in the given list.
Item's car should be a string."
  (cl-labels ((fn-p (entry) (string= (car item) (car entry))))
    (if (cl-remove-if-not (lambda (entry) (fn-p entry)) list)
		(mapcar (lambda (entry) (if (fn-p entry) item entry)) list)
      (cons item list))))

(provide 'api/list)
