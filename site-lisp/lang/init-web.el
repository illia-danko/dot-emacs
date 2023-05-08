;; React and html.
(use-package web-mode :straight t
  :mode (("\\.html\\'". web-mode))
  :commands web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-expanding t))

(define-derived-mode react-mode web-mode "React"
  "A major mode for react editing.")

(mapc (lambda (ext)
        (add-to-list 'auto-mode-alist (cons ext 'react-mode)))
      ["\\.jsx\\'"
       "\\.tsx\\'"])

(use-package js
  :custom
  (js-indent-level 2))

;; Typescript.
(use-package typescript-mode :straight t
  :custom
  (typescript-indent-level 2))

(provide 'init-web)
