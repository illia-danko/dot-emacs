;; Annotations of minibuffer.
(use-package marginalia :straight t
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode))

;; Make minibuffer prettier.
(use-package vertico :straight t
  :ensure t
  :bind (:map vertico-map
              ;; BUG(idanko): M-r is the default vertico
              ;; keybind. Rebind to make it work. Investigate the
              ;; issue.
              ("M-r" . #'vertico-exit-input))
  :custom
  (vertico-count 17)
  (vertico-cycle nil)
  :config
  (vertico-mode 1))

;; Orderless completion engine.
(use-package orderless :straight t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil) ; wish to control everything
  (completion-category-overrides '((file (styles . (partial-completion))))))

;; Templates.
(use-package tempel :straight t
  :init
  (defun my-tempel-setup-capf ()
	;; tampel-complete must be a first item in completion-at-point-functions.
    (add-to-ordered-list 'completion-at-point-functions 'tempel-complete 0))

  :hook
  ((eglot-managed-mode prog-mode text-mode) . my-tempel-setup-capf)

  :custom
  (tempel-path "~/.emacs.d/snippets/*"))

;; Completion backends for corfu.
(use-package cape :straight t
  :after (tempel)
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; current buffer symbols completion
  (add-to-list 'completion-at-point-functions #'cape-file) ; path completion
  )

;; At the cursor completion engline.
(use-package corfu :straight t
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package corfu-terminal :straight t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

;; Based completing-read search/navigation commands.
(use-package consult :straight t
  :bind
  ([remap apropos]                       . consult-apropos)
  ([remap bookmark-jump]                 . consult-bookmark)
  ([remap goto-line]                     . consult-goto-line)
  ([remap imenu]                         . consult-imenu)
  ([remap locate]                        . consult-locate)
  ([remap load-theme]                    . consult-theme)
  ([remap man]                           . consult-man)
  ([remap recentf-open-files]            . consult-recent-file)
  ([remap switch-to-buffer]              . consult-buffer)
  ([remap org-agenda]                    . consult-org-agenda)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
  ([remap yank-pop]                      . consult-yank-pop)
  :custom
  (xref-show-definitions-function 'consult-xref)
  (xref-show-xrefs-function 'consult-xref))

(provide 'init-completion)
