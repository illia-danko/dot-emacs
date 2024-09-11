(require 'project)

;; Uses in conjunction with `project-prefix-map' mapping.
(add-to-list 'project-switch-commands '(tool/vterm-project "VTerm"))
(add-to-list 'project-switch-commands '(tool/magit-status "Magit Status"))
(add-to-list 'project-switch-commands '(tool/rg-project "RipGrep"))
(add-to-list 'project-switch-commands '(project-compile "Build"))

;;;;;;;; Extends project-root to find extra project root patterns  ;;;;;;;;;
;;
;; A credit to https://andreyor.st/posts/2022-07-16-project-el-enhancements

(defcustom core/project-root-markers
  '("Makefile" "go.mod")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun core/project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker core/project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun core/project-find-root (path)
  "Search up the PATH for `core/project-root-markers'."
  (when-let ((root (locate-dominating-file path #'core/project-root-p)))
    (cons 'transient (expand-file-name root))))

(add-to-list 'project-find-functions #'core/project-find-root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core/project-root ()
  (or (ignore-errors (project-root (project-current)))
	  default-directory))

(provide 'core/project)
