(require 'org)

(require 'completion/core)
(require 'tool/version-control)
(require 'api/macro)

(defun extras/org-git-push-org-file ()
  (interactive)
  (tool/git-push-current-file org-directory))

(defun extras/org-consult-ripgrep ()
  (interactive)
  (consult-ripgrep org-directory))

(defun extras/org-capture-slipbox () (interactive) (org-capture nil "x"))

(defun extras/org-toggle-fontifications ()
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

(defun extras/org-consult-ripgrep ()
  (interactive)
  (consult-ripgrep org-directory))

(api/customize-set-variable*
 'org-directory "~/github.com/illia-danko/slipbox"
 'org-default-notes-file (expand-file-name "fleeting_notes.org" org-directory)
 'org-agenda-files (list org-default-notes-file)
 'org-capture-bookmark nil ; do not keep bookmarks
 'org-capture-templates
 `(("x" "[s]lipbox entry" entry (file org-default-notes-file) "* %?\n"))
 'org-fontify-whole-heading-line t
 'org-reverse-note-order t
 'org-fontify-done-headline t ; distinct DONE entries
 'org-fontify-quote-and-verse-blocks t
 'org-hide-emphasis-markers t    ; close links, etc.
 'org-pretty-entities t          ; show LaTeX-like symbols as UTF-8 characters
 'org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done)  ; filter out org-agenda DONE entries
 'org-startup-indented t
 )

(defun extras/org-mode-hook ()
  (outline-hide-body)  ; fold the document on load
  )

(add-hook 'org-mode-hook #'extras/org-mode-hook)

;; Preload babel. Make possible to evaluate src code block.
(org-babel-do-load-languages 'org-babel-load-languages
							 '((shell . t)))


(provide 'extras/org)
