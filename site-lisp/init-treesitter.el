;; Generic syntax highlighter and more.
(use-package tree-sitter :straight t)
  
(use-package tree-sitter-langs :straight t
  :hook ((go-mode) . tree-sitter-hl-mode))

(provide 'init-treesitter)
