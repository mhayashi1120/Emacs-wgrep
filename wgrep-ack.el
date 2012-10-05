;;; wgrep-ack.el --- Writable ack-and-a-half buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Contributor: Ivan Andrus <darthandrus@gmail.com>
;; Keywords: grep edit extensions
;; Package-Requires: ((wgrep "2.1.0") (ack-and-a-half "1.1.0"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ack.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.1.0

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

;; wgrep-ack allows you to edit a ack-and-a-half buffer and apply those
;; changes to the file buffer.

;;; Install:

;; 1. Install ack-and-a-half.el
;;
;;   https://github.com/jhelwig/ack-and-a-half

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'wgrep-ack)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

;;;###autoload
(defun wgrep-ack-setup ()
  ;; ack-and-a-half-mode prints a column number too, so we catch that
  ;; if it exists.  Here \2 is a colon + whitespace separator.  This
  ;; might need to change if (caar grep-regexp-alist) does.
  (set (make-variable-buffer-local 'wgrep-line-file-regexp)
       (concat
        (caar grep-regexp-alist)
        "\\(?:\\([1-9][0-9]*\\)\\2\\)?"))
  (define-key ack-and-a-half-mode-map
    wgrep-enable-key 'wgrep-change-to-wgrep-mode)
  (wgrep-setup-internal))

;;;###autoload(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-setup)
(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-setup)

;; For `unload-feature'
(defun wgrep-ack-unload-function ()
  (remove-hook 'ack-and-a-half-mode-hook 'wgrep-ack-setup))

(provide 'wgrep-ack)

;;; wgrep-ack.el ends here
