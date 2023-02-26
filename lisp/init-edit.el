;; Seamlessly region selection.
(use-package expand-region :straight t
  :bind (("C-o" . er/expand-region)
         ("M-o" . er/contract-region)))

(use-package hydra :straight t)

;; Select everywhere at once.
(use-package multiple-cursors :straight t
  :after (hydra)
  :init
  (defhydra mc/hydra-keymap (:color blue)
    "Multiple Cursors"
    (">" mc/mark-next-like-this "mark next" :exit nil)
    ("n" mc/skip-to-next-like-this "skip to next" :exit nil)
    ("<" mc/unmark-next-like-this "unmark" :exit nil)
    ("m" mc/edit-lines "edit selection" :exit t)
    ("a" mc/mark-all-like-this "mark all" :exit t)
    ("q" nil "cancel"))
  :bind
  (("C-c m" . mc/hydra-keymap/body)))

;; Move the cursor quickly to a word.
(use-package ace-jump-mode :straight t
  :bind
  ("C-q" . ace-jump-mode))

;; Smart parentheses.
(use-package elec-pair
  :config
  (electric-pair-mode 1))

;; Show search results number.
(use-package anzu :straight t
  :hook
  ((prog-mode text-mode) . anzu-mode)
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  :custom
  (anzu-mode-lighter "") ; don't show on modeline
  )

;; Convert the region to isearch prompt.
(use-package isearch
  :init
  (defun my-isearch-region (&rest _)
    "If a region is active, set the selected pattern to isearch input."
    (interactive "P\np")
    (if mark-active
	    (let ((content (buffer-substring-no-properties (mark) (point))))
		  (deactivate-mark)
		  (isearch-yank-string content))))

  :config
  (advice-add 'isearch-forward :after #'my-isearch-region)
  (advice-add 'isearch-backward :after #'my-isearch-region))

(provide 'init-edit)
