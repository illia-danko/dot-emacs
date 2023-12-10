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
;; TODO(idanko): check existence before add to $PATH.
(let ((path `("/usr/local/go/bin"
              "/opt/homebrew/bin"
			  "/usr/local/bin"
              ,(concat (getenv "HOME") "/go/bin")
              ,(concat (getenv "HOME") "/.cargo/bin"))))
  (setq exec-path (append exec-path path))
  (mapc (lambda (p)
          (setenv "PATH" (concat (getenv "PATH") ":" p)))
        path))

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Add files of the 'lisp' folder to the path.
(setq load-path
      (append (delete-dups load-path)
              `(,(expand-file-name "site-lisp" user-emacs-directory))
              `(,(expand-file-name "site-lisp/utils" user-emacs-directory))
              `(,(expand-file-name "site-lisp/core" user-emacs-directory))
              `(,(expand-file-name "site-lisp/completion" user-emacs-directory))
              `(,(expand-file-name "site-lisp/text" user-emacs-directory))
              `(,(expand-file-name "site-lisp/lang" user-emacs-directory))
              `(,(expand-file-name "site-lisp/ui" user-emacs-directory))
              `(,(expand-file-name "site-lisp/tools" user-emacs-directory))
              `(,(expand-file-name "site-lisp/keymap" user-emacs-directory))))

;; Core settings.
(require 'core/intercept-mode)
(require 'core/core)
(require 'core/project)

;; Completion.
(progn
  (straight-use-package 'marginalia)
  (straight-use-package 'vertico)
  (straight-use-package 'which-key)
  (straight-use-package 'orderless)
  (straight-use-package 'cape)
  (straight-use-package 'yasnippet)
  (straight-use-package 'corfu)
  (straight-use-package 'corfu-terminal)
  (straight-use-package 'consult)
  (require 'completion/core)
  (require 'completion/minibuffer)
  (require 'completion/lsp))

;; UI.
(progn
  (straight-use-package 'all-the-icons-completion)
  (straight-use-package 'doom-modeline)
  (straight-use-package 'doom-themes)
  (require 'ui/icons)
  (require 'ui/modeline)
  (require 'ui/theme))

(progn
  (require 'keymap/vanilla))

(message "Load time %.06f" (float-time (time-since my-time-emacs-start)))
