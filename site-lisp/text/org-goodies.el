(require 'text/org)

(progn
  (with-eval-after-load 'org-bullets
	(add-hook 'org-mode-hook #'org-bullets-mode))
  (require 'org-bullets))

(provide 'text/org-goodies)
