(api/customize-set-variable* 'evil-want-keybinding nil) ; required by `evil-collection' to set before evil

(require 'evil)
(require 'evil-surround)
(require 'evil-commentary)
(require 'evil-anzu)
(require 'etm) ; evil-terminal-multiplexer: (Ctrl-h|j|k|l) keymap

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
(api/customize-set-variable* 'evil-collection-mode-list
							 (remove 'vterm evil-collection-mode-list))
(evil-collection-init)

;; evil-surround.
(global-evil-surround-mode 1)

;; evil-commentary.
(evil-commentary-mode 1)

;; `evil-terminal-cursor-changer` alternative.
(unless (display-graphic-p)
  (defun terminal-cursor-shape-bar () (ignore-errors (send-string-to-terminal "\e[6 q")))
  (defun terminal-cursor-shape-box () (ignore-errors (send-string-to-terminal "\e[2 q")))

  (add-hook 'evil-insert-state-entry-hook #'terminal-cursor-shape-bar)
  (add-hook 'evil-insert-state-exit-hook #'terminal-cursor-shape-box)
  (add-hook 'magit-status-headers-hook #'terminal-cursor-shape-box)
  (add-hook 'kill-emacs-hook #'terminal-cursor-shape-box))

(provide 'edit/evil)
