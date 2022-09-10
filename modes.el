;;; modes.el --- Major Modes packages settings -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Elijah Danko
;;
;; Author: Elijah Danko <me@elijahdanko.net>
;; URL: https://github.com/elijahdanko/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Loads and setup major-mode packages I work on a daily basis.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defun trailing-whitespace:show ()
  "Show trailing whitespaces on a buffer."
  (setq-local show-trailing-whitespace t))

(defun prog-mode:hook ()
  (trailing-whitespace:show)
  (hl-line-mode 1)
  (display-line-numbers-mode 1))

(use-package prog-mode
  :hook ((prog-mode . prog-mode:hook)))

(use-package go-mode
  :straight t
  :init
  (defun go-mode:hook ()
    (setq-local comment-fill-column 100
                fill-column 100)
    (unless (eq major-mode 'ediff-mode)
      (eglot-ensure)
      (flycheck-mode))
	(add-hook 'before-save-hook #'gofmt-before-save))
  :hook ((go-mode . go-mode:hook)))

;; It's weird that straight.el doesn't download 's, 'f packages on go-test
;; build.
(use-package s :straight t)
(use-package f :straight t)
(use-package go-test
  :straight t
  :after (go-mode s f)
  :bind (:map go-mode-map
              ("C-c . c" . go-test-current-test)
              ("C-c . t" . go-test-current-file)))

(use-package rainbow-delimiters
  :straight t)

(use-package paredit                    ; better Lisp writing
  :straight t
  :config
  (defun paredit:backward-kill-word-or-region (&optional arg)
    "If mark active acts as `C-w' otherwise as `paredit-backward-kill-word'."
    (interactive)
    (if mark-active
        (call-interactively 'paredit-kill-region)
      (call-interactively 'paredit-backward-kill-word arg)))

  :bind (:map paredit-mode-map
              ("C-w" . paredit:backward-kill-word-or-region)
              ("C-c >" . paredit-forward-slurp-sexp)
              ("C-c <" . paredit-forward-barf-sexp)
              ("C-c u" . paredit-splice-sexp-killing-backward)))

(use-package format-all
  :straight t
  :hook ((format-all-mode . format-all-ensure-formatter)))

(use-package elisp-mode
  :hook ((emacs-lisp-mode . (lambda ()
                              (paredit-mode)
                              (rainbow-delimiters-mode))))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-defun)
              ("C-c C-k" . eval-buffer)))

(use-package flycheck-clj-kondo :straight t)
(use-package clj-refactor :straight t)
(use-package cider
  :init
  (defun clojure-mode:hook ()
    (unless (eq major-mode 'ediff-mode)
      (paredit-mode)
      (clj-refactor-mode)
      (rainbow-delimiters-mode)
      (flycheck-mode)))
  :straight t
  :hook ((clojure-mode . clojure-mode:hook)))

(use-package typescript-mode :straight t)

(use-package js
  :init
  (defun js-mode:hook ()
    ;; Do not enable LSP and linter for *.ts and *.json.
    (let ((ext (file-name-extension buffer-file-name)))
      (and ext
           (pcase ext
             ("ts" t)
             ("js" t)
             ("json" t)
             (_ nil))
           ;; Make sure that LSP is installed:
           ;; sudo npm install -g typescript-language-server
           (unless (eq major-mode 'ediff-mode)
             (unless (string-equal ext "json")
               (eglot-ensure)
               (flycheck-mode))
             (format-all-mode)))))
  :hook ((js-mode . js-mode:hook))
  :bind (:map js-mode-map
              ("M-." . xref-find-definitions)))

(use-package yaml-mode
  :init
  (defun yaml-mode:hook ()
    (prog-mode:hook)
    (unless (eq major-mode 'ediff-mode)
      (flycheck-mode)
      (format-all-mode)))
  :straight t
  :hook ((yaml-mode . yaml-mode:hook)))

(use-package conf-mode
  :hook ((conf-space-mode . prog-mode:hook)))

(use-package protobuf-mode :straight t)

(use-package dockerfile-mode
  :straight t
  :hook ((docker-mode . prog-mode:hook)))

(use-package markdown-mode
  :straight t
  :hook ((markdown-mode . trailing-whitespace:show))
  :config
  (defun markdown:toggle-fontifications (&optional arg)
    "Toggle fontifications on/off."
    (interactive (list (or current-prefix-arg 'toggle)))
    (markdown-toggle-markup-hiding arg))
  :bind (:map markdown-mode-map
              ("C-c w" . markdown-preview)
              ("C-c *" . markdown:toggle-fontifications)))

;; Pretty headings.
(use-package org-superstar :straight t)
;; Required by org-export-to-file.
(use-package htmlize :straight t)

(use-package org
  :init
  (defun org:browser-preview (&optional async subtreep visible-only body-only ext-plist)
    "Preview org file on a browser."
    (interactive)
    (unless (featurep 'ox-html) (require 'ox-html))
    (let* ((dir temporary-file-directory)
           (name (concat (make-temp-name "") ".html"))
           (file (concat (file-name-as-directory dir) name))
           (org-export-coding-system org-html-coding-system))
      (org-open-file (org-export-to-file 'html file
                       async subtreep visible-only body-only ext-plist))))

  (defun org:toggle-fontifications ()
    "Toggle fontifications on/off.
The solution taken from
https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el#L3037-L3069"
    (interactive)
    ;; Toggle markers.
    (setq-local org-hide-emphasis-markers
                (not org-hide-emphasis-markers))
    ;; Toggle links.
    (if org-link-descriptive
        (remove-from-invisibility-spec '(org-link))
      (add-to-invisibility-spec '(org-link)))
    (setq-local org-link-descriptive
                (not org-link-descriptive))
    ;; Apply changes.
    (font-lock-fontify-buffer))

  (defun org:new-todo-entry ()
    "Adds new `TODO' entry."
    (interactive)
    (org-capture nil "n"))

  (defun org:fixup-electric-pairs ()
    ;; Disable electric-pair for `<s' template.
    (when (featurep 'elec-pair)
      (setq-local electric-pair-inhibit-predicate
                  `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

  :hook ((org-mode . (lambda ()
                       (org:fixup-electric-pairs)
                       (org-superstar-mode)
                       (trailing-whitespace:show))))
  :bind (("C-c i" . org:new-todo-entry)
         ("C-c a" . org-todo-list)
         :map org-mode-map
         ("C-c w" . org:browser-preview)
         ("C-c #" . projectile-kill-buffers)
         ("C-c *" . org:toggle-fontifications)
         ("M-RET" . org-table-insert-row)))

(use-package python
  :init
  (defun python-mode:hook ()
    (unless (eq major-mode 'ediff-mode)
      (eglot-ensure)
      (flycheck-mode)))
  :hook ((python-mode . python-mode:hook)))

(use-package sh-script
  :init
  (defun sh-mode:hook ()
    (unless (eq major-mode 'ediff-mode)
      (flycheck-mode)))
  :hook ((sh-mode . sh-mode:hook)))

(use-package restclient :straight t
  :mode ("\\.http\\'" . restclient-mode))

;;; modes.el ends here
