(require 'git-commit)
(require 'text/org)
(require 'tool/version-control)
(require 'edit/evil)

(add-hook 'org-capture-mode-hook #'evil-insert-state)

(provide 'text/org-evil)
