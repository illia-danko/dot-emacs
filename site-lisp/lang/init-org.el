(use-package org :straight t
  :init
  (defun my-org-capture-todo () (interactive) (org-capture nil "t"))
  (defun my-org-capture-diary () (interactive) (org-capture nil "d"))

  (defun my-org-toggle-fontifications ()
    "Toggle fontifications on/off.
The solution taken from
https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el#L3037-L3069"
    (interactive)
    ;; Toggle markers.
    (setq-local org-hide-emphasis-markers
                (not org-hide-emphasis-markers))
    ;; Toggle links.
    (if org-link-descriptive
        (remove-from-invisibility-spec '(org-link))
      (add-to-invisibility-spec '(org-link)))
    (setq-local org-link-descriptive
                (not org-link-descriptive))
    ;; Apply changes.
    (font-lock-fontify-buffer))

  :bind
  (("C-c a" . org-agenda)
   ("C-c o" . my-org-capture-todo)
   ("C-c y" . my-org-capture-diary)
   (:map org-mode-map
         ("C-c 4" . my-org-toggle-fontifications)))

  :custom
  (org-agenda-files (list org-default-notes-file (expand-file-name "diary.org" org-directory)))
  (org-capture-bookmark nil) ; do not keep bookmarks
  (org-capture-templates
   `(("t" "[t]odo item" entry (file org-default-notes-file) "* TODO %?\nEntered on %U")
     ("d" "[d]iary entry" entry (file ,(expand-file-name "diary.org" org-directory)) "* %U %?")))
  (org-default-notes-file (expand-file-name "todos.org" org-directory))
  (org-reverse-note-order t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)    ; close links, etc.
  (org-pretty-entities t)          ; show LaTeX-like symbols as UTF-8 characters

  :config
  ;; Preload babel. Make possible to evaluate src code block.
  (org-babel-do-load-languages 'org-babel-load-languages
							   '(
								 (shell . t)
								 )
							   ))

(provide 'init-org)
