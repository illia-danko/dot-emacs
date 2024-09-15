(api/customize-set-variable* 'evil-want-keybinding nil) ; required by `evil-collection' to set before evil

(require 'evil)
(require 'evil-surround)
(require 'evil-commentary)
(require 'evil-anzu)
(require 'navigate) ; evil-tmux-navigator: (Ctrl-h|j|k|l) keymap

(require 'api/macro)
(require 'core/intercept-mode)

;; evil.
(advice-add 'keyboard-quit :before #'(lambda ()
									   (and evil-mode (evil-force-normal-state))))

(api/customize-set-variable*
 'evil-undo-system 'undo-redo
 'evil-echo-state nil ; do not send state change messages to echo area
 'evil-symbol-word-search t ; search by `word' pattern
 )

(evil-mode 1)

(dolist (state '(normal visual insert))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap core/intercept-mode-map state t t)
   state))

(defalias #'forward-evil-word #'forward-evil-symbol) ; treat underscore or hyper as a word sign based on the current major-mode

;; evil-collection.
(require 'evil-collection)
(evil-collection-init)

;; evil-surround.
(global-evil-surround-mode 1)

;; evil-commentary.
(evil-commentary-mode 1)

;; evil-terminal-cursor-changer substitution.
(add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
(add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q")))

(provide 'edit/evil)
