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

(defun source-file:hook ()
  (trailing-whitespace:show)
  (hl-line-mode 1))

(use-package prog-mode
  :hook ((prog-mode . source-file:hook)))

(use-package compile
  :init
  (defun compile:delete-window-if-successful (buffer string)
    "Bury a compilation buffer and close a compliation window. No-op
on compliation error or warning."
    (when (and
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                        (delete-window))
                      buffer)))
  :config
  (add-hook 'compilation-finish-functions 'compile:delete-window-if-successful))

(use-package go-mode
  :straight t
  :init
  (defun go-mode:setup-buffer ()
    (eglot-ensure)
    (flycheck-mode 1)
	(add-hook 'before-save-hook #'gofmt-before-save))
  :hook ((go-mode . go-mode:setup-buffer)))

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

(use-package elisp-mode
  :hook ((emacs-lisp-mode . (lambda ()
                              (paredit-mode)
                              (rainbow-delimiters-mode))))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-defun)
              ("C-c C-k" . eval-buffer)))

(use-package clj-refactor :straight t)
(use-package cider
  :straight t
  :hook ((clojure-mode . (lambda ()
                           (paredit-mode)
                           (clj-refactor-mode)
                           (rainbow-delimiters-mode)))))

(use-package typescript-mode :straight t)

(use-package prettier-js
  :straight t
  :hook ((js-mode . prettier-js-mode)))

(use-package js
  :init
  :hook ((js-mode . (lambda ()
                      ;; Do not enable LSP and linter for *.ts and *.json.
                      (and buffer-file-name
                           (pcase (file-name-extension buffer-file-name)
                             ("ts" t)
                             ("js" t)
                             (_ nil))
                           ;; Make sure that LSP is installed:
                           ;; sudo npm install -g typescript-language-server
                           (eglot-ensure)
                           (flycheck-mode)))))
  :bind (:map js-mode-map
              ("M-." . xref-find-definitions)))

(use-package yaml-mode
  :straight t
  :hook ((yaml-mode . (lambda ()
                        (flycheck-mode)
                        (source-file:hook)))))

(use-package conf-mode
  :hook ((conf-space-mode . source-file:hook)))

(use-package protobuf-mode :straight t)

(use-package dockerfile-mode
  :straight t
  :hook ((docker-mode . source-file:hook)))

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
              ("C-c o" . markdown-follow-thing-at-point)
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

  (defun org:todo-list ()
    (interactive)
    (org-todo-list)
    (delete-other-windows))

  :hook ((org-mode . (lambda ()
                       (org:fixup-electric-pairs)
                       (org-superstar-mode)
                       (trailing-whitespace:show))))
  :bind (("C-c i" . org:new-todo-entry)
         ("C-c a" . org:todo-list)
         :map org-mode-map
         ("C-c w" . org:browser-preview)
         ("C-c o" . org-open-at-point)
         ("C-c *" . org:toggle-fontifications)
         ("M-RET" . org-table-insert-row)))

(use-package python
  :init
  (defun python-mode:setup-buffer ()
    (eglot-ensure)
    (flycheck-mode 1))
  :hook ((python-mode . python-mode:setup-buffer)))

(use-package yapfify
  :straight '(yapfify
              :type git
              :host github
              :repo "JorisE/yapfify")
  :hook ((python-mode . yapf-mode)))

(use-package sh-script
  :hook ((sh-mode . flycheck-mode)))

;;; modes.el ends here
