;; See https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(defun my-treesit-install-language-grammar ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(use-package elixir-ts-mode :straight t
  :ensure t
  :config
  (advice-add 'my-treesit-install-language-grammar :after (lambda (&rest args)
                                                            (elixir-ts-install-grammar))))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (go-mode . go-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . css-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (heex-mode . heex-ts-mode)
        ))

(provide 'init-treesit)
