;;; api.el --- User Defined Commands and Functions  -*- lexical-binding: t -*-

;; Copyright (c) 2022 Illia Danko
;;
;; Author: Illia Danko <illia@danko.ws>
;; URL: https://github.com/illia-danko/dot-emacs

;; This file is not part of GNU Emacs.

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

(with-eval-after-load
    'company-mode
  (lambda nil
    (setq company-backends
          (mapcar (lambda (backends)
                    (if (and (listp backends) (memq 'company-yasnippet backends))
                        backends
                      (append (if (consp backends)
    	                          backends
                                (list backends))
                              '(:with company-yasnippet))))
                  company-backends))))

(defun region:content ()
  "Takes region content if any."
  (buffer-substring-no-properties (mark) (point)))

(defun region:apply (fn)
  "Apply fn to the marked region text."
  (interactive)
  (if mark-active
	  (let ((content (region:content)))
		(deactivate-mark)
		(funcall fn nil content))
	(funcall fn)))

(defun flyspell:toggle ()
  "Toggle spell checking."
  (interactive)
  (if flyspell-mode
      (progn
        (message "Spell checking off")
        (flyspell-mode -1))
    (progn
      (message "Spell checking on")
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode +1))
      (flyspell-buffer))))

(defun git-link:open-homepage ()
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link-homepage)))

(defun vc:push ()
  "Stage, commit and push upstream a personal note file."
  (interactive)
  (let ((org-dir (expand-file-name org-directory))
        (fullname (buffer-file-name))
        (relname (file-name-nondirectory (buffer-file-name))))
    (when (string-match-p (regexp-quote org-dir) fullname)
      (call-process "git" nil nil nil "add" fullname)
      (call-process "git" nil nil nil "commit" "-m" (format "Update %s" relname))
      (call-process "git" nil nil nil "push")
      (message "Pushed %s" relname))))

(defun dired:system-xdg-open ()
  "Open in dired.
https://www.emacswiki.org/emacs/OperatingOnFilesInDired"
  (interactive)
  (let ((file (dired-get-filename nil t))
        (cmd (pcase system-type
               ('darwin "open")
               (_ "xdg-open"))))
    (call-process cmd nil 0 nil file)))

(defun dired-mode:hook ()
  (dired-hide-details-mode)
  (dired-omit-mode))

(defun isearch:region (&rest _)
  "If a region is active, set a selected pattern as an isearch input."
  (interactive "P\np")
  (if mark-active
	  (let ((content (region:content)))
		(deactivate-mark)
		(isearch-yank-string content))))

(defun user:zen-toggle (&optional arg)
    (interactive)
    (call-interactively 'olivetti-mode arg)
    (call-interactively 'hide-mode-line-mode arg))

(defun emacs:shutdown-server ()
  "Quit Emacs globally. Shutdown server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun trailing-whitespace:show ()
  "Show trailing whitespaces on a buffer."
  (setq-local show-trailing-whitespace t))

(defun prog-mode:hook ()
  (trailing-whitespace:show)
  (hl-line-mode 1)
  (display-line-numbers-mode 1))

(defun go-mode:hook ()
  (setq-local comment-fill-column 150
              fill-column 150)
  (unless (eq major-mode 'ediff-mode)
    (eglot-ensure)
    (flycheck-mode))
  (add-hook 'before-save-hook #'gofmt-before-save))

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

(defun yaml-mode:hook ()
  (prog-mode:hook)
  (unless (eq major-mode 'ediff-mode)
    (flycheck-mode)
    (format-all-mode)))

(defun markdown:toggle-fontifications (&optional arg)
  "Toggle fontifications on/off."
  (interactive (list (or current-prefix-arg 'toggle)))
  (markdown-toggle-markup-hiding arg))

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

(defun python-mode:hook ()
  (unless (eq major-mode 'ediff-mode)
    (eglot-ensure)
    (flycheck-mode)))

(defun sh-mode:hook ()
  (unless (eq major-mode 'ediff-mode)
    (flycheck-mode)))

(defun split-window:jump-right ()
  "Acts as `split-window-right' but also preforms jump to the window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defvar theme:file-path "~/.emacs.d/theme"
  "Emacs theme filepath.")

(defvar theme:default-name "doom-one-light"
  "Current Emacs theme.")

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun theme:save-to-file (path name)
  (with-temp-buffer
    (insert name)
    (write-region (point-min) (point-max) path)))

(defun theme:save-current-to-flie ()
  (theme:save-to-file theme:file-path
                      (symbol-name (car custom-enabled-themes))))

(defun theme:ensure-exists ()
  (unless (file-exists-p theme:file-path)
    (theme:save-to-file theme:file-path theme:default-name)))

(defun theme:load-from-file ()
  (theme:ensure-exists)
  (load-theme
   (intern
    (string-trim
     (with-temp-buffer
       (insert-file-contents theme:file-path)
       (buffer-string))))
   t))

(add-hook 'after-load-theme-hook #'theme:save-current-to-flie)

(defun theme:update-faces (&optional frame)
  "Adjust faces."
  (when frame
    (select-frame frame))
  (theme:load-from-file))

(add-hook 'after-init-hook #'theme:update-faces)
(add-hook 'after-make-frame-functions #'theme:update-faces)

;; Maximize window on startup.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'after-make-frame-functions
          (lambda (&optional frame)
	        (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))

(defun evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

;;; api.el ends here
