
(require 'expand-region)

(progn
  (require 'hydra)
  (require 'multiple-cursors)

  (defhydra edit/multiple-cursors-keymap (:color blue)
    "Multiple Cursors"
    (">" mc/mark-next-like-this "mark next" :exit nil)
    ("n" mc/skip-to-next-like-this "skip to next" :exit nil)
    ("<" mc/unmark-next-like-this "unmark" :exit nil)
    ("m" mc/edit-lines "edit selection" :exit t)
    ("a" mc/mark-all-like-this "mark all" :exit t)
    ("q" nil "cancel")))

(provide 'edit/vanilla)
