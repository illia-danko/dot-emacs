(use-package dired
  :init
  (defun my-dired-system-open ()
    (interactive)
    (let ((file (dired-get-filename nil t))
          (cmd (pcase system-type
                 ('darwin "open")
                 (_ "xdg-open"))))
      (call-process cmd nil 0 nil file)))

  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)

  :bind
  ([remap dired] . dired-jump)
  (:map dired-mode-map
        ("O" . my-dired-system-open)))

(provide 'init-dired)
