(progn
  (with-eval-after-load 'org
    (customize-set-variable 'org-directory "~/codeberg.org/eli87/org")
    (customize-set-variable 'org-default-notes-file (expand-file-name "todos.org" org-directory))
    (customize-set-variable 'org-agenda-files (list org-default-notes-file (expand-file-name "diary.org" org-directory)))
    (customize-set-variable 'org-capture-bookmark nil) ; do not keep bookmarks
    (customize-set-variable 'org-capture-templates
			    `(("t" "[t]odo item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")
			      ("d" "[d]iary entry" entry (file ,(expand-file-name "diary.org" org-directory)) "* %U %?")))
    (customize-set-variable 'org-reverse-note-order t)
    (customize-set-variable 'org-fontify-done-headline t) ; distinct DONE entries
    (customize-set-variable 'org-fontify-quote-and-verse-blocks t)
    (customize-set-variable 'org-hide-emphasis-markers t)    ; close links, etc.
    (customize-set-variable 'org-pretty-entities t)          ; show LaTeX-like symbols as UTF-8 characters
    (customize-set-variable 'org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done)) ; filter out org-agenda DONE entries
    )

  (require 'org))

(provide 'text/org)
