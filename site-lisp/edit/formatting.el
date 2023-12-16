(defvar edit/formatting-prettier-config-path (concat
											  "--config="
											  (expand-file-name "~/.config/prettier/prettier.config.js"))
  "Prettier config path.")

(progn
  (with-eval-after-load 'format-all
    (customize-set-variable 'format-all-show-errors 'never)
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))

  (require 'format-all))

(provide 'edit/formatting)
