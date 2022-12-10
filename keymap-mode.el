
(add-hook
 'rg-mode-hook
 (lambda nil
   (define-key rg-mode-map (kbd "C-c C-s") #'wgrep-save-all-buffers)))

(add-hook
 'go-mode-hook
 (lambda nil
   (define-key go-mode-map (kbd "C-c . c") #'go-test-current-test)
   (define-key go-mode-map (kbd "C-c . t") #'go-test-current-file)))

(add-hook
 'paredit-mode-hook
 (lambda nil
   (define-key paredit-mode-map (kbd "C-w") #'paredit:backward-kill-word-or-region)
   (define-key paredit-mode-map (kbd "C-c >") #'paredit-forward-slurp-sexp)
   (define-key paredit-mode-map (kbd "C-c <") #'paredit-forward-barf-sexp)
   (define-key paredit-mode-map (kbd "C-c u") #'paredit-splice-sexp-killing-backward)))

(add-hook
 'js-mode-hook
 (lambda nil
   (define-key js-mode-map (kbd "M-.") #'xref-find-definitions)))

(add-hook
 'markdown-mode-hook
 (lambda nil
   (define-key markdown-mode-map (kbd "C-c w") #'markdown-preview)
   (define-key markdown-mode-map (kbd "C-c *") #'markdown:toggle-fontifications)))

(add-hook
 'org-mode-hook
 (lambda nil
   (define-key org-mode-map (kbd "C-c w") #'org:browser-preview)
   (define-key org-mode-map (kbd "C-c w") #'projectile-kill-buffers)
   (define-key org-mode-map (kbd "C-c w") #'org:toggle-fontifications)
   (define-key org-mode-map (kbd "M-RET") #'org-table-insert-row)))

(add-hook
 'evil-mode-hook
 (lambda nil
   (define-key evil-normal-state-map   (kbd "C-g") #'evil-keyboard-quit)
   (define-key evil-motion-state-map   (kbd "C-g") #'evil-keyboard-quit)
   (define-key evil-insert-state-map   (kbd "C-g") #'evil-keyboard-quit)
   (define-key evil-window-map         (kbd "C-g") #'evil-keyboard-quit)
   (define-key evil-operator-state-map (kbd "C-g") #'evil-keyboard-quit)
   ))
