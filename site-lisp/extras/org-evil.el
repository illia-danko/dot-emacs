(require 'edit/evil)

(add-hook 'org-capture-mode-hook #'evil-insert-state)

(provide 'extras/org-evil)
