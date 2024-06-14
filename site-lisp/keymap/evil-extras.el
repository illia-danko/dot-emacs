(require 'extras/org)
(require 'extras/org-evil)

(evil-define-key* '(normal) org-mode-map
  (kbd "RET") #'org-open-at-point)

(provide 'keymap/evil-extras)
