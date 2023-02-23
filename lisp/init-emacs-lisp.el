(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . #'eval-defun)
        ("C-c C-b" . #'eval-buffer))
  (:map lisp-interaction-mode-map
        ("C-c C-c" . #'eval-defun)
        ("C-c C-b" . #'eval-buffer)))

(provide 'init-emacs-lisp)
