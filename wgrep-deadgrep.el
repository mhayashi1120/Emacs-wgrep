;;; wgrep-deadgrep.el --- Writable deadgrep buffer and apply the changes to files -*- lexical-binding: t -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>, Iku Iwasa <iku.iwasa@gmail.com>
;; Keywords: grep edit extensions
;; Package-Requires: ((wgrep "2.3.0") (emacs "25.1"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-deadgrep.el
;; Emacs: GNU Emacs 25 or later
;; Version: 2.3.1

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

;; wgrep-deadgrep allows you to edit a deadgrep buffer and apply those changes to
;; the file buffer.

;;; Install:

;; 1. Install deadgrep.el
;;
;;   https://github.com/Wilfred/deadgrep

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (autoload 'wgrep-deadgrep-setup "wgrep-deadgrep")
;;     (add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

(defun wgrep-deadgrep-prepare-header&footer ()
  "Prepare header in `deadgrep' buffer for `wgrep'.
Since `deadgrep' does not have footer, only header is handled."
  ;; `deadgrep' uses "deadgrep-filename" property for filename.
  (let ((pos (next-single-property-change (point-min) 'deadgrep-filename)))
    (if pos
        (add-text-properties (point-min) pos '(read-only t wgrep-header t))
      (add-text-properties (point-min) (point-max)
                           '(read-only t wgrep-header t)))))

(defun wgrep-deadgrep-parse-command-results ()
  "Parse `deadgrep' results for `wgrep'."
  ;; Note that this function is called with the buffer narrowed to
  ;; exclude the header and the footer.  (We're going to assert that
  ;; fact here, because we use (bobp) result a bit further down to
  ;; decide that we're not reading grouped results; see below.)
  (unless (bobp)
    (error "Expected to be called with point at beginning of buffer"))
  (save-excursion
    (while (not (eobp))
      (let* ((pos (point))
             (filename (get-text-property pos 'deadgrep-filename))
             (line (get-text-property pos 'deadgrep-line-number)))
        (when filename
          (if line
              (let* ((eol (line-end-position))
                     (end (next-single-property-change
                           pos 'deadgrep-line-number nil eol)))
                (add-text-properties pos end
                                     (list 'wgrep-line-filename filename
                                           'wgrep-line-number line)))
            ;; Ignore the line that introduces matches from a file, so that
            ;; wgrep doesn't let you edit it.
            (add-text-properties
             pos (line-end-position)
             (list 'wgrep-ignore t
                   (wgrep-construct-filename-property filename)
                   filename)))))
      (forward-line 1))))

;;;###autoload
(defun wgrep-deadgrep-setup ()
  "Setup `wgrep-deadgrep' for `deadgrep'."
  (setq wgrep-prepared nil)
  (set (make-local-variable 'wgrep-header&footer-parser)
       'wgrep-deadgrep-prepare-header&footer)
  (set (make-local-variable 'wgrep-results-parser)
       'wgrep-deadgrep-parse-command-results)
  (wgrep-setup-internal))

;;;###autoload
(add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup)

;; For `unload-feature'
(defun wgrep-deadgrep-unload-function ()
  "Unload `wgrep-deadgrep' setup."
  (remove-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup))

(provide 'wgrep-deadgrep)

;;; wgrep-deadgrep.el ends here
