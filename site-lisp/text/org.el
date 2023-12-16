(require 'completion/core)
(require 'tool/version-control)

(defun text/org-git-push-org-file ()
  (interactive)
  (tool/git-push-current-file org-directory))

(defun text/org-consult-ripgrep ()
  (interactive)
  (consult-ripgrep org-directory))

(defun text/org-capture-todo () (interactive) (org-capture nil "t"))
(defun text/org-capture-diary () (interactive) (org-capture nil "d"))

(defun text/org-toggle-fontifications ()
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

(defun text/org-consult-ripgrep ()
  (interactive)
  (consult-ripgrep org-directory))

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

  (add-hook 'org-mode-hook #'outline-hide-other) ; fold the document on load

  ;; Preload babel. Make possible to evaluate src code block.
  (org-babel-do-load-languages 'org-babel-load-languages
							   '((shell . t))))

(require 'org)

(provide 'text/org)
