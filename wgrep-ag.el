;;; wgrep-ag.el --- Writable ag buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: grep edit extensions
;; Package-Requires: ((wgrep "2.1.5"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.1.3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; wgrep-ag allows you to edit a ag buffer and apply those changes to
;; the file buffer.

;;; Install:

;; 1. Install ag.el
;;
;;   https://github.com/Wilfred/ag.el

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (autoload 'wgrep-ag-setup "wgrep-ag")
;;     (add-hook 'ag-mode-hook 'wgrep-ag-setup)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

;;;###autoload
(defun wgrep-ag-setup ()
  ;; ag.el prints a column number too, so we catch that
  ;; if it exists.  Here \2 is a colon + whitespace separator.  This
  ;; might need to change if (caar grep-regexp-alist) does.
  (set (make-local-variable 'wgrep-line-file-regexp)
       (concat
        wgrep-default-line-header-regexp
        "\\(?:\\([1-9][0-9]*\\)\\2\\)?"))
  (wgrep-setup-internal))

;;;###autoload
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;; For `unload-feature'
(defun wgrep-ag-unload-function ()
  (remove-hook 'ag-mode-hook 'wgrep-ag-setup))

(provide 'wgrep-ag)

;;; wgrep-ag.el ends here
