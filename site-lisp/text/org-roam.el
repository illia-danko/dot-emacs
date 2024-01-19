(require 'org-roam)
(require 'org-roam-protocol)
(require 'api/macro)
(require 'completion/core)

;; org-roam.
(api/customize-set-variable*
 'org-roam-directory (file-truename "~/github.com/illia-danko/org-roam")
 'org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
 'org-roam-completion-everywhere t
 'org-roam-capture-templates '(("d" "default" plain "%?" :target
								(file+head "${slug}.org" "#+title: ${title}\n#+filetags: \n")
								:unnarrowed t)))

(add-to-list 'org-link-frame-setup '(file . find-file))
(org-roam-db-autosync-mode 1)

(defun text/org-roam-consult-ripgrep ()
  (interactive)
  (consult-ripgrep org-roam-directory))

(provide 'text/org-roam)
