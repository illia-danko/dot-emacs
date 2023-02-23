(defvar my-prettier-config-path (concat
                              "--config="
                              (expand-file-name "~/.config/prettier/prettier.config.js"))
  "Prettier config path.")

(provide 'init-vars)
