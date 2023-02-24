;; Always open symlinks without confirmation.
(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

;; Workhorse git client.
(use-package magit :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1) ; magit uses the whole frame space
  (magit-diff-refine-hunk 'all)  ; word-wise diff highlight

  :bind
  ("C-c g?" . #'magit-blame-addition)
  ("C-c gd" . #'magit-diff-buffer-file)
  ("C-c gl" . #'magit-log-all)
  ("C-c gb" . #'magit-log-buffer-file))

;; Copy/open git urls.
(use-package git-link :straight t
  :init
  (defun my-git-link-open-page ()
    (interactive)
    (let ((git-link-open-in-browser t))
      (call-interactively 'git-link-homepage)))

  :bind (("C-c gu" . git-link)
         ("C-c gU" . #'my-git-link-open-page)))

;; Emacs diff tool.
(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally) ; split buffers horizontally
  )

(provide 'init-vc)
