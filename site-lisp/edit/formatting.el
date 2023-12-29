(require 'format-all)
(require 'api/macro)

;; Vars.
(defvar edit/formatting-prettier-config-path (concat
											  "--config="
											  (expand-file-name "~/.config/prettier/prettier.config.js"))
  "Prettier config path.")

;; format-all.
(api/customize-set-variable* 'format-all-show-errors 'never)
(add-hook 'format-all-mode-hook #'format-all-ensure-formatter)


(provide 'edit/formatting)
