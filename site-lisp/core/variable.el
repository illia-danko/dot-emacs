(require 'api/variable)

(with-eval-after-load 'emacs
  (api/customize-set-variable*
   'tab-width 4  ; number spaces per a tab
   'ring-bell-function 'ignore ; stop ring bell alarms
   'fill-column 100 ; 100 characters per a line
   'comment-fill-column 100
   'set-mark-command-repeat-pop t ; do not repeat C-u prefix on mark commands (i.e. C-u C-SPC)
   'warning-minimum-level :error ; do not show warnings
   'truncate-lines t ; do not wrap long lines
   'tab-always-indent 'complete
   'enable-local-variables :all ; run .dir-locals.el with no dialog
   'mac-command-modifier 'meta ; use command key as meta
   ;; Karabiner-elments already remap opt to ctrl
   ;; (cusotomize-set-variable 'mac-option-modifier 'control)
   'vc-follow-symlinks t) ; always follow a symlink when accessing a file
  )

(provide 'core/variable)
