(require 'edit/evil)
(require 'core/intercept-mode)
(require 'tool/vterm-evil)
(require 'text/org-evil)

;; Core.
(global-set-key [remap evil-copy-from-above] #'yank)
(define-key evil-normal-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-motion-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-insert-state-map   (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-window-map         (kbd "C-g") #'edit/evil-keyboard-quit)
(define-key evil-operator-state-map (kbd "C-g") #'edit/evil-keyboard-quit)

(evil-define-key* '(normal) vterm-copy-mode-map
  (kbd "i") #'tool/vterm-evil-exit-copy-mode
  (kbd "I") #'tool/vterm-evil-exit-copy-mode
  (kbd "a") #'tool/vterm-evil-exit-copy-mode
  (kbd "A") #'tool/vterm-evil-exit-copy-mode
  (kbd "o") #'tool/vterm-evil-exit-copy-mode
  (kbd "O") #'tool/vterm-evil-exit-copy-mode)

(evil-define-key* '(normal) org-mode-map
  (kbd "RET") #'org-open-at-point)

(provide 'keymap/evil)
