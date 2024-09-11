;; Git gutter.
(require 'git-gutter)

(defun tool/git-gutter-popup-hunk-jump (&optional diffinfo)
  (interactive)
  (git-gutter:popup-hunk diffinfo)
  (switch-to-buffer-other-window git-gutter:popup-buffer))

(api/customize-set-variable*
 'git-gutter:ask-p nil
 'left-margin-width 1; add space for a git-gutter sign
 'git-gutter:added-sign ""
 'git-gutter:deleted-sign ""
 'git-gutter:modified-sign "")

(global-git-gutter-mode 1)

(provide 'tool/gutter)
