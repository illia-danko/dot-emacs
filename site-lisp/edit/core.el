(require 'ace-jump-mode)
(require 'rainbow-delimiters)
(require 'expand-region)
(require 'elec-pair)
(require 'anzu)
(require 'isearch)

;; Core.
(defun edit/backward-kill-word-or-region (&optional arg)
  "If mark is active acts as `C-w' otherwise as `backward-kill-word'."
  (interactive "p")
  (if mark-active
      (kill-region (mark) (point))
    (backward-kill-word arg)))

;; elec-pair.
(add-to-list 'electric-pair-pairs '(?` . ?`)) ;; add backtick symbol (`)
(electric-pair-mode 1)

;; anzu.
(global-anzu-mode 1)

;; isearch.
(defun edit/isearch-region (&rest _)
  "If any region is active, set a isearch input to the selected pattern."
  (interactive "P\np")
  (if mark-active
	  (let ((content (buffer-substring-no-properties (mark) (point))))
		(deactivate-mark)
		(isearch-yank-string content))))

(advice-add 'isearch-forward :after #'edit/isearch-region)
(advice-add 'isearch-backward :after #'edit/isearch-region)


(provide 'edit/core)
