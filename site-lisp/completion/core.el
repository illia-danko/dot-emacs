(require 'api/macro)

;; which-key-mode.
(require 'which-key)
(which-key-mode 1)

(require 'tempel)
(require 'tempel-collection)
(api/customize-set-variable*
 'tempel-trigger-prefix "<")

(defun completion/snippets-setup-capf ()
  (setq-local completion-at-point-functions
			  (cons #'tempel-complete completion-at-point-functions)))

(add-hook 'conf-mode-hook 'completion/snippets-setup-capf)
(add-hook 'prog-mode-hook 'completion/snippets-setup-capf)
(add-hook 'text-mode-hook 'completion/snippets-setup-capf)

;; cape & orderless.
(require 'cape)
(require 'orderless)
(api/customize-set-variable*
 'completion-styles '(orderless flex)
 'completion-category-defaults nil
 'completion-category-overrides '((file (styles . (partial-completion)))))

(add-to-list 'completion-at-point-functions #'cape-dabbrev) ; current buffer symbols completion
(add-to-list 'completion-at-point-functions #'cape-file) ; path completion

;; corfu.
(require 'corfu)
(require 'corfu-terminal)
(api/customize-set-variable*
 'corfu-popupinfo-delay 0
 'corfu-preview-current nil
 'corfu-quit-at-boundary nil ; never quit at completion boundary
 'corfu-quit-no-match t ; quit, if there is no match
 'corfu-preselect 'directory ; preselect a first candidate, except directory
 'corfu-on-exact-match nil) ; configure handling of exact matches

(global-corfu-mode 1)
(corfu-popupinfo-mode 1) ; add doc string next to corfu completion popup

;; corfu-terminal.
(unless (display-graphic-p)
  (corfu-terminal-mode 1))

;; consult.
(require 'consult)
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

(defun completion/consult-line-multi ()
  (interactive)
  (if mark-active
	  (let* ((content (buffer-substring-no-properties (mark) (point))))
		(deactivate-mark)
		(consult-line-multi content content))
	(consult-line-multi nil)))

(api/customize-set-variable*
 'xref-show-definitions-function 'consult-xref
 'xref-show-xrefs-function 'consult-xref
 'consult-preview-key '(:debounce 0.4 any)
 'consult-ripgrep (concat "rg "
						  (getenv "RG_OPTS_FILTER")
						  " --null --line-buffered --color=never --max-columns=1000 --path-separator"
						  " --smart-case --no-heading --with-filename --line-number --search-zip")
 'consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer consult--source-file-register)
 'consult-project-buffer-sources '(consult--source-project-buffer)
 )

(add-to-list 'consult-preview-allowed-hooks 'display-line-numbers-mode)

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(require 'eldoc)
(api/customize-set-variable* 'eldoc-echo-area-use-multiline-p nil)
(global-eldoc-mode 1)

(provide 'completion/core)
