(use-package emacs
  :init
  (defun my-backward-kill-word-or-region (&optional arg)
	"If mark is active acts as `C-w' otherwise as `backward-kill-word'."
	(interactive "p")
	(if mark-active
		(kill-region (mark) (point))
      (backward-kill-word arg)))

  :bind
  ("C-w" . my-backward-kill-word-or-region)
  ([remap kill-buffer] . kill-this-buffer))

(provide 'init-personal)
