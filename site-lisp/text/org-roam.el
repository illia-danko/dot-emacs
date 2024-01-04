(require 'org-roam)
(require 'org-roam-protocol)
(require 'api/macro)

(api/customize-set-variable*
 'org-roam-directory (file-truename "~/github.com/illia-danko/org-roam")
 'org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
 'org-roam-completion-everywhere t)

(add-to-list 'org-link-frame-setup '(file . find-file))

(org-roam-db-autosync-mode 1)

(provide 'text/org-roam)
