(use-package flycheck :straight t
  :init
  (defun my-flycheck-mode ()
    (unless (eq major-mode 'ediff-mode)
      (flycheck-mode)))

  :custom
  (flycheck-indication-mode nil) ; don't show fringe indicator

  ;; yaml-mode - powered by `yamllint'
  ;; sh-mode   - powered by `shellcheck'
  :hook ((yaml-mode sh-mode) . my-flycheck-mode))

(provide 'init-codelinter)
