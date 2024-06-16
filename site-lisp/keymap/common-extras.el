(require 'extras/org)
(require 'extras/org-roam)

(define-key org-mode-map (kbd "<C-tab>") #'org-cycle)
(define-key org-mode-map (kbd "C-c 4") #'extras/org-toggle-fontifications)
(define-key org-mode-map (kbd "C-c gc") #'extras/org-git-push-org-file)
(define-key core/intercept-mode-map (kbd "C-c oa") #'org-agenda)
(define-key core/intercept-mode-map (kbd "C-c ot") #'extras/org-capture-todo)
(define-key core/intercept-mode-map (kbd "C-c od") #'extras/org-capture-diary)

;; Org Roam.
(define-key core/intercept-mode-map (kbd "C-c ol") #'org-roam-backlinks-and-links)
(define-key core/intercept-mode-map (kbd "C-c of") #'org-roam-node-find)
(define-key core/intercept-mode-map (kbd "C-c og") #'org-roam-graph)
(define-key core/intercept-mode-map (kbd "C-c oi") #'org-roam-node-insert)
(define-key core/intercept-mode-map (kbd "C-c oc") #'org-roam-capture)
(define-key core/intercept-mode-map (kbd "C-c oC") #'org-roam-dailies-capture-today)
(define-key core/intercept-mode-map (kbd "C-c os") #'extras/org-roam-consult-ripgrep)

;; org-download.
(define-key org-mode-map (kbd "C-c i") #'org-download-clipboard)

(provide 'keymap/common-extras)
