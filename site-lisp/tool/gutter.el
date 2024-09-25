;; Git gutter fringe UI version.
(require 'git-gutter-fringe)

(defun tool/git-gutter-popup-hunk-jump (&optional diffinfo)
  (interactive)
  (git-gutter:popup-hunk diffinfo)
  (switch-to-buffer-other-window git-gutter:popup-buffer))

;; Enable fringe on the right side.
(fringe-mode '(0 . 8))

(mapc (lambda (fringe-face)
		(fringe-helper-define fringe-face nil
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"
							  "....XXXX"))
      [git-gutter-fr:added git-gutter-fr:deleted git-gutter-fr:modified])

(api/customize-set-variable*
 'git-gutter-fr:side 'right-fringe
 'git-gutter:ask-p nil)

(global-git-gutter-mode 1)

(provide 'tool/gutter)
