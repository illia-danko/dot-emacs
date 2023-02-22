(use-package magit :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-diff-refine-hunk 'all))

(provide 'init-vc)
