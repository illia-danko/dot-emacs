(require 'extras/org)
(require 'extras/org-evil)
(require 'api/macro)

(evil-define-key* '(normal) org-mode-map
  (kbd "RET") #'org-open-at-point
  (kbd "g kb") #'org-roam-backlinks-and-links
  (kbd "g kf") #'org-roam-node-find
  (kbd "g kg") #'org-roam-graph
  (kbd "g ki") #'org-roam-node-insert)

(evil-define-key* '(insert) org-mode-map
  (kbd "C-v") #'org-roam-node-insert)

(evil-define-key* '(normal) core/intercept-mode-map
  (kbd "g ka") #'org-agenda
  (kbd "g kx") #'extras/org-capture-slipbox
  (kbd "g kv")  (defun-iteractive org-default-nodes (find-file org-default-notes-file)))

(provide 'keymap/evil-extras)
