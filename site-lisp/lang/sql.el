(require 'edit/formatting)

;; (setq format-all-default-formatters
;; 	  (api/upsert-car-string format-all-default-formatters
;; 							 '("SQL" pgformatter)))

(add-hook 'sql-mode-hook #'format-all-mode)

(provide 'lang/sql)
