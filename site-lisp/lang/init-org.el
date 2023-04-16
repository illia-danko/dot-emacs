(use-package org :straight t
  :init
  (defun my-org-capture-todo () (interactive) (org-capture nil "t"))
  (defun my-org-capture-diary () (interactive) (org-capture nil "d"))

  :bind
  (("C-c o" . org-agenda)
   ("C-c u" . my-org-capture-todo)
   ("C-c y" . my-org-capture-diary))

  :custom
  (org-agenda-files (list org-default-notes-file (expand-file-name "diary.org" org-directory)))
  (org-capture-bookmark nil) ; do not keep bookmarks
  (org-capture-templates
   `(("t" "[t]odo item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")
     ("d" "[d]iary entry" entry (file ,(expand-file-name "diary.org" org-directory)) "* %U %?")))
  (org-default-notes-file (expand-file-name "todos.org" org-directory))
  (org-reverse-note-order t)

  :config
  ;; Preload babel. Make possible to evaluate src code block.
  (org-babel-do-load-languages 'org-babel-load-languages
							   '(
								 (shell . t)
								 )
							   ))

(provide 'init-org)
