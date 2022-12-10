;;; keymap.el --- Emacs User Interface -*- lexical-binding: t -*-

(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-x !") #'emacs:shutdown-server)
(global-set-key (kbd "C-w") #'edit:backward-kill-word-or-region)
(global-set-key (kbd "C-l") #'edit:mark-line)
(global-set-key (kbd "M-l") #'recenter-top-bottom)
(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "C-c t h") #'hl-line-mode)
(global-set-key (kbd "C-c t s") #'flyspell:toggle)
(global-set-key (kbd "C-c C-z") #'toggle-read-only)

(global-set-key (kbd "C-c g g") #'magit-status)
(global-set-key (kbd "C-c g b") #'magit-blame-addition)
(global-set-key (kbd "C-c g l") #'magit-log-buffer-file)
(global-set-key (kbd "C-c g c") #'vc:push)
(global-set-key (kbd "C-c g u") #'git-link)

(global-set-key (kbd "C-c g o") #'git-link:open-homepage)
(global-set-key (kbd "M-y") #'consult-yank-from-kill-ring)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-c h") #'consult-recent-file)
(global-set-key (kbd "C-c /") #'consult-imenu)
(global-set-key (kbd "C-c s") #'(lambda () (interactive) (region:apply 'consult-ripgrep)))
(global-set-key (kbd "C-c n") #'(lambda () (interactive) (consult-ripgrep org-directory)))
(global-set-key (kbd "C-c f") #'consult-find)

(global-set-key (kbd "C-\\") #'embark-dwim)
(global-set-key (kbd "M-|") #'embark-act)
(global-set-key (kbd "C-h b") #'embark-bindings)

(global-set-key (kbd "C-c p") #'projectile-switch-project)
(global-set-key (kbd "C-c #") #'projectile-kill-buffers)
(global-set-key (kbd "C-c ~") #'projectile-remove-known-project)

(global-set-key (kbd "C-c TAB") #'company-complete)

(global-set-key (kbd "C-o") #'er/expand-region)
(global-set-key (kbd "M-o") #'er/contract-region)

(define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c e r") #'eglot-find-implementation)

(defhydra mc:hydra-keymap (:color blue)
  "Multiple Cursors"
  (">" mc/mark-next-like-this "mark next" :exit nil)
  ("n" mc/skip-to-next-like-this "skip to next" :exit nil)
  ("<" mc/unmark-next-like-this "unmark" :exit nil)
  ("m" mc/edit-lines "edit selection" :exit t)
  ("a" mc/mark-all-like-this "mark all" :exit t)
  ("q" nil "cancel"))
(global-set-key (kbd "C-c m") #'mc:hydra-keymap/body)

(global-set-key (kbd "C-x d") #'dired-jump)

(define-key dired-mode-map "o" #'dired:system-xdg-open)
(define-key dired-mode-map (kbd "C-c c") nil)

(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

(global-set-key (kbd "C-c t l") #'display-line-numbers-mode)
(global-set-key (kbd "C-c SPC") #'distraction-free-toggle)
(global-set-key (kbd "C-c ~") #'elfeed)

(global-set-key (kbd "C-x 3") #'split-window:jump-right)
(global-set-key (kbd "C-c t c") #'rainbow-mode)

(add-hook
 'rg-mode
 (lambda nil
   (define-key rg-mode-map (kbd "C-c C-s") #'wgrep-save-all-buffers)))

(add-hook
 'go-test
 (lambda nil
   (define-key go-mode-map (kbd "C-c . c") #'go-test-current-test)
   (define-key go-mode-map (kbd "C-c . t") #'go-test-current-file)))

(add-hook
 'paredit-mode
 (lambda nil
   (define-key paredit-mode-map (kbd "C-w") #'paredit:backward-kill-word-or-region)
   (define-key paredit-mode-map (kbd "C-c >") #'paredit-forward-slurp-sexp)
   (define-key paredit-mode-map (kbd "C-c <") #'paredit-forward-barf-sexp)
   (define-key paredit-mode-map (kbd "C-c u") #'paredit-splice-sexp-killing-backward)))

(add-hook
 'js-mode
 (lambda nil
   (define-key js-mode-map (kbd "M-.") #'xref-find-definitions)))

(add-hook
 'markdown-mode
 (lambda nil
   (define-key markdown-mode-map (kbd "C-c w") #'markdown-preview)
   (define-key markdown-mode-map (kbd "C-c *") #'markdown:toggle-fontifications)))

(global-set-key (kbd "C-c i") #'org:new-todo-entry)
(global-set-key (kbd "C-c a") #'org-todo-list)

(add-hook
 'org-mode
 (lambda nil
   (define-key org-mode-map (kbd "C-c w") #'org:browser-preview)
   (define-key org-mode-map (kbd "C-c w") #'projectile-kill-buffers)
   (define-key org-mode-map (kbd "C-c w") #'org:toggle-fontifications)
   (define-key org-mode-map (kbd "M-RET") #'org-table-insert-row)))

;;; keymap.el ends here
