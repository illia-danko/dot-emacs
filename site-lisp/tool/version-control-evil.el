(require 'git-commit)
(require 'tool/version-control)

(add-hook 'git-commit-setup-hook #'evil-insert-state)

(provide 'tool/version-control-evil)
