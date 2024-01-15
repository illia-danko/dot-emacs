(require 'typescript-ts-mode)
(require 'js)

(require 'api/macro)
(require 'edit/treesit)
(require 'completion/lsp)
(require 'edit/formatting)

;; Common settings.
(cl-pushnew '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode) . ("typescript-language-server" "--stdio"))
            eglot-server-programs
            :test #'equal)

;; HTML.
(add-to-list 'treesit-language-source-alist '(html "https://github.com/tree-sitter/tree-sitter-html"))

;; React.
(add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))

;; Typescript.
(add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'typescript-ts-mode-hook #'format-all-mode)

;; Javascript.
(api/customize-set-variable* 'js-indent-level 2)
(add-hook 'js-mode-hook #'eglot-ensure)
(add-hook 'js-mode-hook #'format-all-mode)

(provide 'lang/web)
