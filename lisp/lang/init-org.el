(use-package org :straight t
  :bind
  (("C-c ol" . #'org-todo-list)
   ("C-c oa" . #'org-agenda)
   ("C-c ot" . (lambda nil (interactive) (org-capture nil "n")))
   ("C-c od" . (lambda nil (interactive (org-capture nil "d"))))
   ("C-c oc" . #'org-capture))

  :custom
  (org-directory "~/github.com/illia-danko/org")
  (org-agenda-files (list org-default-notes-file (expand-file-name "diary.org" org-directory)))
  (org-capture-bookmark nil) ; do not keep bookmarks
  (org-capture-templates
   `(("n" "[n]ew TODO item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")
     ("d" "[d]iary entry" entry (file (expand-file-name "diary.org" org-directory)) "* %U %?")))
  (org-default-notes-file (expand-file-name "todos.org" org-directory)))

(provide 'init-org)
