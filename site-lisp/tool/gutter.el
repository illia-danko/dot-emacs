;; Git gutter.
(require 'git-gutter)

(defun tool/git-gutter-popup-hunk-jump (&optional diffinfo)
  (interactive)
  (git-gutter:popup-hunk diffinfo)
  (switch-to-buffer-other-window git-gutter:popup-buffer))

(defconst tool/gutter-sign "")

(api/customize-set-variable*
 'git-gutter:ask-p nil
 'git-gutter:added-sign tool/gutter-sign
 'git-gutter:deleted-sign tool/gutter-sign
 'git-gutter:modified-sign tool/gutter-sign)

(global-git-gutter-mode 1)

(git-gutter:set-window-margin (length tool/gutter-sign))

(provide 'tool/gutter)
