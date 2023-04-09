(defvar my-time-emacs-start (current-time)
  "Time Emacs has started.")

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      load-prefer-newer t
      font-lock-verbose nil
      byte-compile-verbose nil)

(add-hook 'emacs-startup-hook
		  (lambda ()
			;; After startup, it is important you reset this to some reasonable
			;; default. A large gc-cons-threshold will cause freezing and
			;; stuttering during long-term interactive use."
            (setq gc-cons-threshold 16777216
				  gc-cons-percentage 0.1)))

;; Adjust Emacs $PATH. To make $PATH works correctly on Emacs GUI it's needed to
;; set via both: `exec-path' and `setenv'.
(let ((path `("/usr/local/go/bin"
              "/opt/homebrew/bin"
			  "/usr/local/bin"
              ,(concat (getenv "HOME") "/go/bin"))))
  (setq exec-path (append exec-path path))
  (mapc (lambda (p)
          (setenv "PATH" (concat (getenv "PATH") ":" p)))
        path))

;; Add files of the 'lisp' folder to the path.
(setq load-path
      (append (delete-dups load-path)
              `(,(expand-file-name "site-lisp" user-emacs-directory))
			  `(,(expand-file-name "site-lisp/lang" user-emacs-directory))))

(require 'init-bootstrap)
(require 'init-vars)
(require 'init-core)
(require 'init-completion)
(require 'init-formatting)
(require 'init-codelinter)
(require 'init-edit)
(require 'init-vc)
(require 'init-dired)
(require 'init-tools)
(require 'init-appearance)

;; Load languages modes.
(require 'init-conf)
(require 'init-dockermode)
(require 'init-emacs-lisp)
(require 'init-org)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-go)
(require 'init-ts-tsx)

(message "Load time %.06f" (float-time (time-since my-time-emacs-start)))
