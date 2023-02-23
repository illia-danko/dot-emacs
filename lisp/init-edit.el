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
  :bind (("C-c m" . mc/hydra-keymap/body)))

;; Smart parentheses.
(use-package elec-pair
  :config
  (electric-pair-mode 1))

(provide 'init-edit)
