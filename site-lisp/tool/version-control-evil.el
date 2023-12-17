(progn
  (with-eval-after-load 'git-commit
	(add-hook 'git-commit-setup-hook #'evil-insert-state))

  (require 'git-commit))

(provide 'tool/version-control-evil)
