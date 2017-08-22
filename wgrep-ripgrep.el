;;; wgrep-ripgrep.el --- Writable ripgrep buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>, Bailey Ling <bling@live.ca>
;; Keywords: grep edit extensions
;; Package-Requires: ((wgrep "2.1.5"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ripgrep.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.0.1

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

;; wgrep-ripgrep allows you to edit a ripgrep buffer and apply those changes to
;; the file buffer.

;;; Install:

;; 1. Install ripgrep.el
;;
;;   https://github.com/nlamirault/ripgrep.el

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (autoload 'wgrep-ripgrep-setup "wgrep-ripgrep")
;;     (add-hook 'ripgrep-search-mode-hook 'wgrep-ripgrep-setup)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

;;;###autoload
(defun wgrep-ripgrep-setup ()
  (wgrep-setup-internal))

;;;###autoload
(add-hook 'ripgrep-search-mode-hook 'wgrep-ripgrep-setup)

;; For `unload-feature'
(defun wgrep-ripgrep-unload-function ()
  (remove-hook 'ripgrep-search-mode-hook 'wgrep-ripgrep-setup))

(provide 'wgrep-ripgrep)

;;; wgrep-ripgrep.el ends here
