(with-eval-after-load 'emacs
  (customize-set-variable 'tab-width 4)  ; number spaces per a tab
  (customize-set-variable 'ring-bell-function 'ignore) ; stop ring bell alarms
  (customize-set-variable 'fill-column 100) ; 100 characters per a line
  (customize-set-variable 'comment-fill-column 100)
  (customize-set-variable 'set-mark-command-repeat-pop t) ; do not repeat C-u prefix on mark commands (i.e. C-u C-SPC)
  (customize-set-variable 'warning-minimum-level :error) ; do not show warnings
  (customize-set-variable 'truncate-lines t) ; do not wrap long lines
  (customize-set-variable 'tab-always-indent 'complete)
  (customize-set-variable 'enable-local-variables :all) ; run .dir-locals.el with no dialog
  (customize-set-variable 'mac-command-modifier 'meta) ; use command key as meta
  ;; Karabiner-elments already remap opt to ctrl
  ;; (cusotomize-set-variable 'mac-option-modifier 'control)

  (fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
  (put 'upcase-region 'disabled nil) ; don't confirm on upcase command
  (put 'downcase-region 'disabled nil) ; don't confirm on downcase command
  (column-number-mode 1) ; show column number on modeline
  )

(provide 'core/variable)
