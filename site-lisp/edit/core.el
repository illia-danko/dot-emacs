(defun edit/backward-kill-word-or-region (&optional arg)
  "If mark is active acts as `C-w' otherwise as `backward-kill-word'."
  (interactive "p")
  (if mark-active
      (kill-region (mark) (point))
    (backward-kill-word arg)))

(progn
  (with-eval-after-load 'elec-pair
    (add-to-list 'electric-pair-pairs '(?` . ?`)) ;; add backtick symbol (`)
    (electric-pair-mode 1))

  (require 'elec-pair))

(provide 'edit/core)
