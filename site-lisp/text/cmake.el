(require 'edit/treesit)

(add-to-list 'treesit-language-source-alist '(cmake "https://github.com/uyha/tree-sitter-cmake"))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))

(provide 'text/cmake)
