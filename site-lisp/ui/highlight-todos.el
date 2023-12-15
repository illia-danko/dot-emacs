(progn
  (with-eval-after-load 'hl-todo
	(global-hl-todo-mode 1))

  (require 'hl-todo))

(provide 'ui/highlight-todos)
