(require 'api/variable)
(require 'core/intercept-mode)

(progn
  (with-eval-after-load 'evil
	(defun edit/evil-keyboard-quit ()
      "Keyboard quit and force normal state."
      (interactive)
      (and evil-mode (evil-force-normal-state))
      (keyboard-quit))

	(api/customize-set-variable*
	 'evil-undo-system 'undo-redo
	 'evil-echo-state nil ; do not send state change messages to echo area
	 'evil-symbol-word-search t) ; search by `word' pattern

	(evil-mode 1)

	(dolist (state '(normal visual insert))
      (evil-make-intercept-map
       (evil-get-auxiliary-keymap core/intercept-mode-map state t t)
       state))

	(defalias #'forward-evil-word #'forward-evil-symbol) ; treat underscore or hyper as a word sign based on the current major-mode
	)

  (api/customize-set-variable* 'evil-want-keybinding nil) ; required by `evil-collection' to set before evil.
  (require 'evil))

(progn
  (with-eval-after-load'evil-collection
   (evil-collection-init))
  (require 'evil-collection))

(progn
  (with-eval-after-load 'evil-surround
	(global-evil-surround-mode 1))

  (require 'evil-surround))

(progn
  (with-eval-after-load 'evil-commentary
	(evil-commentary-mode 1))

  (require 'evil-commentary))

(progn
  (with-eval-after-load 'evil-terminal-cursor-changer
	(unless (display-graphic-p)
	  (etcc-on)))

  (require 'evil-terminal-cursor-changer))

(require 'evil-anzu)
(require 'navigate) ; evil-tmux-navigator

(provide 'edit/evil)
