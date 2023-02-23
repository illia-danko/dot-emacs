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
              `(,(expand-file-name "lisp" user-emacs-directory))))

(require 'init-bootstrap)
(require 'init-vars)
(require 'init-core)
(require 'init-completion)
(require 'init-formatting)
(require 'init-lsp)
(require 'init-treesitter)
(require 'init-edit)
(require 'init-rg)
(require 'init-vc)
(require 'init-dired)
(require 'init-org)
(require 'init-markdown)
(require 'init-emacs-lisp)
(require 'init-go)
(require 'init-yaml)
(require 'init-dockermode)
(require 'init-restclient)
(require 'init-personal)
(require 'init-theme)

(message "Load time %.06f" (float-time (time-since my-time-emacs-start)))
