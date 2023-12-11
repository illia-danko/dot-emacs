(progn
  (with-eval-after-load 'which-key
    (which-key-mode 1))

  (require 'which-key))

(progn
  (with-eval-after-load 'orderless
    (customize-set-variable 'completion-styles '(orderless flex))
    (customize-set-variable 'completion-category-defaults nil)
    (customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion))))))

  (require 'orderless))

(progn
  (with-eval-after-load 'cape
    (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; current buffer symbols completion
    (add-to-list 'completion-at-point-functions #'cape-file) ; path completion
    )

  (require 'cape))

(progn
  (defun completion/snippets-setup-capf ()
    ;; tampel-complete must be a first item in completion-at-point-functions.
    (add-to-ordered-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet) 0))

  (with-eval-after-load 'yasnippet
    (yas-global-mode 1))

  (require 'yasnippet))

(progn
  (with-eval-after-load 'corfu
    (customize-set-variable 'corfu-auto t) ; automatically trigger popups
    (customize-set-variable 'corfu-popupinfo-delay 0)
    (customize-set-variable 'corfu-preview-current nil)
    (customize-set-variable 'corfu-quit-at-boundary nil) ; never quit at completion boundary
    (customize-set-variable 'corfu-quit-no-match t) ; quit, if there is no match
    (customize-set-variable 'corfu-preselect 'prompt) ; preselect the prompt
    (customize-set-variable 'corfu-on-exact-match nil) ; configure handling of exact matches

    (global-corfu-mode 1)
    (corfu-popupinfo-mode 1) ; add doc string next to corfu completion popup
    )

  (require 'corfu))

(unless (display-graphic-p)
  (with-eval-after-load 'corfu-terminal
    (corfu-terminal-mode 1))

  (require 'corfu-terminal))

(progn
  (defun completion/apply-region (func &rest args)
	"Apply the given `func' to its `args' and the marked region.
If is no region, calls `func' without any `args'."
	(if mark-active
        (let* ((content (buffer-substring-no-properties (mark) (point)))
			   (args (append args `(,content))))
          (deactivate-mark)
          (apply 'funcall func args))
      (funcall func)))

  (defun completion/consult-ripgrep ()
    (interactive)
    (completion/apply-region 'consult-ripgrep
			     (core/project-root)))

  (defun completion/consult-line ()
    (interactive)
    (completion/apply-region 'consult-line))

  (defun completion/consult-ripgrep-org ()
    (interactive)
    (consult-ripgrep org-directory))

  (with-eval-after-load 'consult
    (customize-set-variable 'xref-show-definitions-function 'consult-xref)
    (customize-set-variable 'xref-show-xrefs-function 'consult-xref)
    (customize-set-variable 'consult-ripgrep "rg \
--hidden -g !{.git,.svn,.hg,CVS,.bzr,vendor,node_modules,dist,venv,elm-stuff,.clj-kondo,.lsp,.cpcache} \
--null --line-buffered --color=never --max-columns=1000 --path-separator / \
--smart-case --no-heading --with-filename --line-number --search-zip"))

  (require 'consult))

(provide 'completion/core)
