(require 'eglot)
(require 'dumb-jump)
(require 'flymake)
(require 'api/macro)

(api/customize-set-variable* 'eglot-stay-out-of '(flymake))

;; Override `eglot--message' to disable minibuffer messages.
(defun eglot--message (format &rest args))

;; Override `eglot-inlay-hints-mode' to avoid warning of the not used feature.
(defun eglot-inlay-hints-mode (&optional mode))

;; ---- Use dumb-jump as a fallback. -----
;; (See https://github.com/joaotavora/eglot/issues/420#issuecomment-1257247512)

(advice-add 'eglot-xref-backend :override 'xref-eglot+dumb-backend)

(defun xref-eglot+dumb-backend () 'eglot+dumb)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
  (cons (xref-backend-identifier-at-point 'eglot)
        (xref-backend-identifier-at-point 'dumb-jump)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
  (xref-backend-identifier-completion-table 'eglot))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-definitions 'eglot (car identifier))
      (xref-backend-definitions 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-references ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-references 'eglot (car identifier))
      (xref-backend-references 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot+dumb)) pattern)
  (xref-backend-apropos 'eglot pattern))

;; ---- end -----

(provide 'completion/lsp)
