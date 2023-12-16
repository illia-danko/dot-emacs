(defvar core/time-emacs-start (current-time)
  "Time Emacs has started.")

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      load-prefer-newer t
      font-lock-verbose nil
      byte-compile-verbose nil)

(add-hook 'emacs-startup-hook
		  #'(lambda ()
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
  (mapc #'(lambda (p)
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
              `(,(expand-file-name "site-lisp" user-emacs-directory))))

;; Extensible and customizable VI Layer for Emacs.
(defvar core/use-evil-p t)

;; Core settings.
(progn
  (straight-use-package 'perspective)
  (require 'core/variable)
  (require 'core/intercept-mode)
  (require 'core/core)
  (require 'core/project)
  (require 'core/scope))

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
  (straight-use-package 'embark-consult)
  (require 'completion/core)
  (require 'completion/minibuffer)
  (require 'completion/lsp))

;; Tool.
(progn
  (straight-use-package 'xclip)
  (straight-use-package 'dashboard)
  (straight-use-package 'vterm)
  (straight-use-package 'magit)
  (straight-use-package 'git-link)
  (straight-use-package 'elfeed)
  (require 'tool/core)
  (require 'tool/dashboard)
  (require 'tool/vterm)
  (require 'tool/filesystem)
  (require 'tool/spelling)
  (require 'tool/version-control)
  (require 'tool/autoclose-compile-window)
  (require 'tool/diff)
  (require 'tool/feed))

;; Editing.
(progn
  (straight-use-package 'format-all)
  (straight-use-package 'ace-jump-mode)
  (straight-use-package 'expand-region)
  (straight-use-package 'multiple-cursors)
  (straight-use-package 'hydra)
  (straight-use-package 'anzu)
  (straight-use-package 'rainbow-delimiters)
  (straight-use-package 'evil)
  (straight-use-package 'evil-collection)
  (straight-use-package 'evil-anzu)
  (straight-use-package 'evil-surround)
  (straight-use-package 'evil-commentary)
  (straight-use-package 'evil-terminal-cursor-changer)
  (straight-use-package
   '(navigate :type git :host codeberg :repo "eli87/evil-tmux-navigator"))
  (require 'edit/core)
  (require 'edit/formatting)
  (require 'edit/treesit)
  (if core/use-evil-p
      (require 'edit/evil)
	(require 'edit/vanilla)))

;; Text.
(progn
  (straight-use-package 'org-bullets)
  (straight-use-package 'markdown-mode)
  (require 'text/text-mode)
  (require 'text/org)
  (require 'text/org-goodies)
  (require 'text/markdown)
  (require 'text/yaml)
  )

;; Languages.
(progn
  (straight-use-package 'elixir-ts-mode)
  (straight-use-package 'inf-elixir)
  (require 'lang/prog-mode)
  (require 'lang/emacs-lisp)
  (require 'lang/go)
  (require 'lang/elixir))

;; UI.
(progn
  (straight-use-package 'all-the-icons-completion)
  (straight-use-package 'doom-modeline)
  (straight-use-package 'doom-themes)
  (straight-use-package 'rainbow-mode)
  (straight-use-package 'hl-todo)
  (straight-use-package 'hide-mode-line)
  (straight-use-package 'olivetti)
  (require 'ui/face)
  (require 'ui/icons)
  (require 'ui/modeline)
  (require 'ui/system-theme)
  (require 'ui/font)
  (require 'ui/rgb-highlight)
  (require 'ui/highlight-todos)
  (require 'ui/zen))

;; Keymap.
(progn
  (require 'keymap/common)
  (if core/use-evil-p
      (require 'keymap/evil)
    (require 'keymap/vanilla)))

(message "Load time %.06f" (float-time (time-since core/time-emacs-start)))
