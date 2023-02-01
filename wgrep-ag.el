;;; wgrep-ag.el --- Writable ag buffer and apply the changes to files -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020,2023 Masahiro Hayashi

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: grep edit extensions
;; Package-Requires: ((wgrep "3.0.0"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el
;; Emacs: GNU Emacs 25 or later
;; Version: 3.0.0

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

(defvar wgrep-ag-grouped-result-file-regexp "^File:[[:space:]]+\\(.*\\)$"
  "Regular expression for the start of results for a file in grouped results.
\"Grouped results\" are what you get from ag.el when
`ag-group-matches' is true or when you call ag with --group.")

(defvar wgrep-ag-ungrouped-result-regexp
  "^\\(.+?\\):\\([[:digit:]]+\\)\\(?:-\\|:[[:digit:]]+:\\)"
  "Regular expression for an ungrouped result.
You get \"ungrouped results\" when `ag-group-matches' is false or
when you manage to call ag with --nogroup.")

(defun wgrep-ag-prepare-header/footer ()
  (save-excursion
    (goto-char (point-min))
    ;; Look for the first useful result line.
    (if (re-search-forward (concat wgrep-ag-grouped-result-file-regexp
                                   "\\|"
                                   wgrep-ag-ungrouped-result-regexp))
        (add-text-properties (point-min) (line-beginning-position)
                             '(read-only t wgrep-header t))
      ;; No results in this buffer, let's mark the whole thing as
      ;; header.
      (add-text-properties (point-min) (point-max)
                           '(read-only t wgrep-header t)))

    ;; OK, header dealt with. Now let's try find the footer.
    (goto-char (point-max))
    (re-search-backward "^\\(?:-[^:]+?:[[:digit:]]+:[[:digit:]]+:\\)" nil t)
    ;; Point is now at the beginning of the result nearest the end
    ;; of the buffer, AKA the last result.  Move to the start of
    ;; the line after the last result, and mark everything from
    ;; that line forward as wgrep-footer.  If we can't move to the
    ;; line after the last line then there apparently is no
    ;; footer.
    (when (zerop (forward-line 1))
      (add-text-properties (point) (point-max)
                           '(read-only t wgrep-footer t)))))

(defun wgrep-ag-parse-command-results ()
  ;; Note that this function is called with the buffer narrowed to
  ;; exclude the header and the footer.  (We're going to assert that
  ;; fact here, because we use (bobp) result a bit further down to
  ;; decide that we're not reading grouped results; see below.)
  (unless (bobp)
    (error "Expected to be called with point at beginning of buffer"))
  (save-excursion
    ;; First look for grouped results (`ag-group-matches' is/was
    ;; probably true).
    (while (re-search-forward wgrep-ag-grouped-result-file-regexp nil t)
      ;; Ignore the line that introduces matches from a file, so that
      ;; wgrep doesn't let you edit it.
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(wgrep-ignore t))
      (let ((file-name (match-string-no-properties 1)))
        ;; Note that I think wgrep uses this property to quickly find
        ;; the file it's interested in when searching during some
        ;; operation(s).  We stick it on the file name in the results
        ;; group header.
        (add-text-properties (match-beginning 1) (match-end 1)
                             (list (wgrep-construct-filename-property file-name)
                                   file-name))
        ;; Matches are like: 999:55:line content here
        ;; Context lines are like: 999-line content here
        ;;
        ;; When context is enabled, matches from different parts of
        ;; the same file are separated by a line containing just "--".
        ;; The group of matches from a single file is terminated by a
        ;; blank line.
        (while (and (zerop (forward-line 1))
                    (looking-at
                     (concat "^\\([[:digit:]]+\\)\\(?::[[:digit:]]+:\\|-\\)"
                             "\\|\\(^--$\\)")))
          (if (match-beginning 2)
              ;; Ignore "--" line.
              (add-text-properties (match-beginning 0) (match-end 0)
                                   '(wgrep-ignore t))
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'wgrep-line-filename file-name
                                       'wgrep-line-number
                                       (string-to-number (match-string 1))))))))
    (when (bobp)
      ;; Search above never moved point, so match non-grouped results
      ;; (`ag-group-matches' is/was probably false).
      (let (last-file-name)
        ;; Matches are like: /foo/bar:999:55:line content here
        ;; Context lines are like: /foo/bar:999-line content here
        ;;
        ;; With context lines, matches from different parts of the
        ;; file are separated by a line containing just "--".
        (while (re-search-forward (concat wgrep-ag-ungrouped-result-regexp
                                          "\\|\\(^--$\\)")
                                  nil t)
          (if (match-beginning 3)
              ;; Ignore the "--" separator.
              (add-text-properties (match-beginning 0) (match-end 0)
                                   '(wgrep-ignore t))
            (let ((file-name (match-string-no-properties 1))
                  (line-number (string-to-number (match-string 2))))
              (unless (equal file-name last-file-name)
                ;; This line is a result from a different file than
                ;; the last match (or else this is the first match in
                ;; the results).  Write the special file name property
                ;; for wgrep.
                (let ((file-name-prop
                       (wgrep-construct-filename-property file-name)))
                  (add-text-properties (match-beginning 1) (match-end 1)
                                       (list file-name-prop file-name)))
                (setq last-file-name file-name))
              (add-text-properties (match-beginning 0) (match-end 0)
                                   (list 'wgrep-line-filename file-name
                                         'wgrep-line-number line-number)))))))))

;;;###autoload
(defun wgrep-ag-setup ()
  (set (make-local-variable 'wgrep-header/footer-parser)
       'wgrep-ag-prepare-header/footer)
  (set (make-local-variable 'wgrep-results-parser)
       'wgrep-ag-parse-command-results)
  (wgrep-setup-internal))

;;;###autoload
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;; For `unload-feature'
(defun wgrep-ag-unload-function ()
  (remove-hook 'ag-mode-hook 'wgrep-ag-setup))

(provide 'wgrep-ag)

;;; wgrep-ag.el ends here
