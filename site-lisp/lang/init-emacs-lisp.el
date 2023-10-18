(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . #'eval-defun)
        ("C-c C-b" . #'eval-buffer))
  (:map lisp-interaction-mode-map
        ("C-c C-c" . #'eval-defun)
        ("C-c C-b" . #'eval-buffer)))

(advice-add 'eval-buffer :after (lambda (&rest args) (message "Buffer Evaluation - Done")))

(provide 'init-emacs-lisp)
