(require 'api/variable)

(progn
  (with-eval-after-load 'ediff
	(defun tool/compare-two-open-windows ()
      (interactive)
      (let* ((windows (window-list))
			 (buffer-a (window-buffer (nth 1 windows)))
			 (buffer-b (window-buffer (nth 2 windows))))
		(ediff-buffers buffer-a buffer-b)))

	(api/customize-set-variable*
	 'ediff-split-window-function 'split-window-horizontally ; split buffers horizontally
	 'ediff-window-setup-function 'ediff-setup-windows-plain) ; use one frame for diff
	)

  (require 'ediff))

(provide 'tool/diff)
