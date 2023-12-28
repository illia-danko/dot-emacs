(require 'which-key)
(require 'orderless)
(require 'cape)
(require 'yasnippet)
(require 'corfu)
(require 'corfu-terminal)
(require 'consult)
(require 'embark-consult)

(require 'api/variable)

;; which-key-mode.
(which-key-mode 1)

;; cape.
(api/customize-set-variable*
 'completion-styles '(orderless flex)
 'completion-category-defaults nil
 'completion-category-overrides '((file (styles . (partial-completion)))))

(add-to-list 'completion-at-point-functions #'cape-dabbrev) ; current buffer symbols completion
(add-to-list 'completion-at-point-functions #'cape-file) ; path completion

(defun completion/snippets-setup-capf ()
  ;; tampel-complete must be a first item in completion-at-point-functions.
  (add-to-ordered-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet) 0))

;; yasnippet.
(yas-global-mode 1)

;; corfu.
(api/customize-set-variable*
 'corfu-auto t ; automatically trigger popups
 'corfu-popupinfo-delay 0
 'corfu-preview-current nil
 'corfu-quit-at-boundary nil ; never quit at completion boundary
 'corfu-quit-no-match t ; quit, if there is no match
 'corfu-preselect 'prompt ; preselect the prompt
 'corfu-on-exact-match nil) ; configure handling of exact matches

(global-corfu-mode 1)
(corfu-popupinfo-mode 1) ; add doc string next to corfu completion popup

;; corfu-terminal.
(unless (display-graphic-p)
  (corfu-terminal-mode 1))

;; consult.
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
 'consult-ripgrep "rg \
--hidden -g !{.git,.svn,.hg,CVS,.bzr,vendor,node_modules,dist,venv,elm-stuff,.clj-kondo,.lsp,.cpcache} \
--null --line-buffered --color=never --max-columns=1000 --path-separator / \
--smart-case --no-heading --with-filename --line-number --search-zip")

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(provide 'completion/core)
