(use-package web-mode :straight t
  :mode (("\\.js\\'". web-mode)
		 ("\\.jsx\\'". web-mode)
		 ("\\.html\\'". web-mode))
  :commands web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-expanding t))

;;;###autoload
(define-derived-mode typescriptreact-mode web-mode "TypescriptReact"
  "A major mode for tsx.")

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ts\\'" 'typescriptreact-mode))
(add-to-list 'auto-mode-alist (cons "\\.tsx\\'" 'typescriptreact-mode))

(provide 'init-ts-tsx)
