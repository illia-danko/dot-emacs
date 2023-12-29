(require 'wgrep)
(require 'rg)
(require 'api/macro)

(api/customize-set-variable* 'wgrep-auto-save-buffer t) ; automatically save buffers after edit

(provide 'tool/rg)
