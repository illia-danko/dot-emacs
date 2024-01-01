(require 'org-roam)
(require 'org-roam-protocol)
(require 'api/macro)

(api/customize-set-variable*
 'org-roam-directory (file-truename "~/github.com/illia-danko/org-roam")
 'org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(provide 'text/org-roam)
