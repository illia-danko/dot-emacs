;;; abbrevs.el --- Auto-correct typos and abbreviation insertion -*- lexical-binding: t -*-
;;
;; Copyright (c) 2021 Illia A. Danko
;;
;; Author: Illia A. Danko <illia@idanko.net>
;; URL: https://github.com/idanko/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; It uses for auto-correction and basic abbreviation rather than text
;; snippets insertion.

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

(require 'abbrev)

(define-abbrev-table 'global-abbrev-table
  ;; Handle some typos.
  '(("wether" "whether")))

;;; abbrevs.el ends here
