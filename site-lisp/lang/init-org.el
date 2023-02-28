(use-package org :straight t
  :init
  (defun my-org-capture-todo () (interactive) (org-capture nil "t"))
  (defun my-org-capture-diary () (interactive) (org-capture nil "d"))

  :bind
  (("C-c ol" . org-todo-list)
   ("C-c oa" . org-agenda)
   ("C-c ot" . my-org-capture-todo)
   ("C-c od" . my-org-capture-diary)
   ("C-c oc" . org-capture))

  :custom
  (org-directory "~/github.com/illia-danko/org")
  (org-agenda-files (list org-default-notes-file (expand-file-name "diary.org" org-directory)))
  (org-capture-bookmark nil) ; do not keep bookmarks
  (org-capture-templates
   `(("t" "[t]odo item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")
     ("d" "[d]iary entry" entry (file ,(expand-file-name "diary.org" org-directory)) "* %U %?")))
  (org-default-notes-file (expand-file-name "todos.org" org-directory))

  :config
  ;; Preload babel. Make possible to evaluate src code block.
  (org-babel-do-load-languages 'org-babel-load-languages
							   '(
								 (shell . t)
								 )
							   ))

(provide 'init-org)
