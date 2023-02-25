(defvar my-time-emacs-start (current-time)
  "Time Emacs has started.")

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      load-prefer-newer t
      font-lock-verbose nil
      byte-compile-verbose nil)

(add-hook 'emacs-startup-hook
		  (lambda ()
			;; After startup, it is important you reset this to some
			;; reasonable default. A large gc-cons-threshold will
			;; cause freezing and stuttering during long-term
			;; interactive use."
            (setq gc-cons-threshold 16777216
				  gc-cons-percentage 0.1)))

;; Add files of the 'lisp' folder to the path.
(setq load-path
      (append (delete-dups load-path)
              `(,(expand-file-name "lisp" user-emacs-directory))
			  `(,(expand-file-name "lisp/lang" user-emacs-directory))))

(require 'init-bootstrap)
(require 'init-vars)
(require 'init-core)
(require 'init-completion)
(require 'init-formatting)
(require 'init-codelinter)
(require 'init-lsp)
(require 'init-treesitter)
(require 'init-edit)
(require 'init-vc)
(require 'init-dired)
(require 'init-personal)
(require 'init-tools)
(require 'init-theme)

;; Load languages modes.
(require 'init-conf)
(require 'init-dockermode)
(require 'init-emacs-lisp)
(require 'init-org)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-go)

(message "Load time %.06f" (float-time (time-since my-time-emacs-start)))
