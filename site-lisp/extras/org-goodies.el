(require 'org-bullets)
(require 'extras/org)
(require 'org-download)
(require 'api/macro)

;; org-bullets.
(add-hook 'org-mode-hook #'org-bullets-mode)

;; org-download.
(api/customize-set-variable*
 'org-download-method 'directory
 'org-download-image-dir "images"
 'org-download-annotate-function #'(lambda (&rest _) "") ; do not annotate
 'org-download-heading-lvl nil)

(add-hook 'dired-mode-hook 'org-download-enable)

(provide 'extras/org-goodies)
