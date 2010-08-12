;;; wgrep --- Make writable grep buffer and apply the changes to files
;; -*- Mode: Emacs-Lisp -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: grep edit
;; URL: http://github.com/mhayashi1120/wgrep-el/raw/master/wgrep.el

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

;; wgrep provides to edit grep buffer and to apply the changes to
;; the file.
;;

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'wgrep)

;; This program is forked version. Original version can be downloaded from
;; http://www.bookshelf.jp/elc/grep-edit.el

;; Following added implementations and differences.
;; * Support grep option -A (--after-context) -B (--before-context)
;; * Some bugfix. (wrong coloring text etc..)
;; * wdired like interface.
;; * Remove all advise.
;; * Bind to local variables.

;; Usage:
;; You can edit the text on *grep* buffer after type C-c C-p.
;; After that the changed text is highlighted.
;; Then, type C-c C-e to apply the highlighting changes
;; to files.

;; C-c C-e : apply the highlighting changes to files.
;; C-c C-u : All changes are ignored
;; C-c C-r : Remove the highlight in the region (The Changes doesn't
;; apply to files. Of course, if you type C-c C-e, the remained
;; highlight changes are applied to files.)


;;; History:
;; 

;;; Code:

(require 'grep)

(defgroup wgrep nil
  "Customize wgrep"
  :group 'grep)

(defcustom wgrep-change-readonly-file nil
  "*Non-nil means to change read only files."
  :group 'wgrep
  :type 'boolean)

(defface wgrep-face
  '((((class color)
      (background dark))
     (:background "SlateGray1" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "*Face used for the changed text on grep buffer."
  :group 'wgrep)

(defface wgrep-file-face
  '((((class color)
      (background dark))
     (:background "gray30" :bold t))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "*Face used for the changed text on file buffer."
  :group 'wgrep)

(defface wgrep-reject-face
  '((((class color)
      (background dark))
     (:foreground "hot pink" :bold t))
    (((class color)
      (background light))
     (:foreground "red" :bold t))
    (t
     ()))
  "*Face used for the line on grep buffer that can not apply to file."
  :group 'wgrep)

(defface wgrep-done-face
  '((((class color)
      (background dark))
     (:foreground "LightSkyBlue" :bold t))
    (((class color)
      (background light))
     (:foreground "blue" :bold t))
    (t
     ()))
  "*Face used for the line on grep buffer that can apply to file."
  :group 'wgrep)

(defvar wgrep-overlays nil)
(defvar wgrep-file-overlays nil)
(defvar wgrep-reject-overlays nil)
(defvar wgrep-done-overlays nil)

(make-variable-buffer-local 'wgrep-overlays)
(make-variable-buffer-local 'wgrep-file-overlays)
(make-variable-buffer-local 'wgrep-reject-overlays)
(make-variable-buffer-local 'wgrep-done-overlays)

(defconst wgrep-line-file-regexp (caar grep-regexp-alist))

(add-hook 'grep-setup-hook
          (lambda ()
            (define-key grep-mode-map "\C-c\C-p" 'wgrep-to-wgrep-mode)
	    (set (make-local-variable 'compilation-finish-function) 'wgrep-finish-function)))

(defvar wgrep-mode-map nil)
(unless wgrep-mode-map
  (setq wgrep-mode-map
	(let ((map (make-sparse-keymap)))

	  (define-key map "\C-c\C-c" 'wgrep-finish-edit)
	  (define-key map "\C-c\C-e" 'wgrep-finish-edit)
	  (define-key map "\C-c\C-r" 'wgrep-remove-change)
	  (define-key map "\C-c\C-u" 'wgrep-remove-all-change)
	  (define-key map "\C-x\C-s" 'wgrep-finish-edit)
	  (define-key map "\C-m"     'ignore)
	  (define-key map "\C-j"     'ignore)
	  (define-key map "\C-o"     'ignore)

	  map)))

(defun wgrep-set-readonly-area (state)
  (let ((inhibit-read-only t)
	(regexp (format "\\(?:%s\\|\n\\)" wgrep-line-file-regexp))
	beg end)
    (save-excursion
      (setq beg (point-min))
      (wgrep-goto-first-found)
      (setq end (point))
      (put-text-property beg end 'read-only state)
      (while (re-search-forward regexp nil t)
        (put-text-property (match-beginning 0)
                           (match-end 0) 'read-only state)))))

(defun wgrep-mode-change-face (beg end leng-before)
  (when (and (wgrep-process-exited-p)
	     (/= beg (point-min))) ;; ignore when first executing
    (let ((ovs (overlays-in beg end))
	  (exist-ovelays nil)
	  ov)
      (while ovs
	(if (overlay-get (car ovs) 'wgrep)
	    (setq exist-ovelays t))
	(setq ovs (cdr ovs)))
      (unless exist-ovelays
	(setq ov
	      (make-overlay
	       (line-beginning-position)
	       (+ 1 (line-end-position))))
	(overlay-put ov 'wgrep t)
	(overlay-put ov 'face 'wgrep-face)
	(overlay-put ov 'priority 0)
	(setq wgrep-overlays (cons ov wgrep-overlays))))))

(defun wgrep-get-info ()
  (beginning-of-line)
  (when (looking-at (concat wgrep-line-file-regexp "\\([^\n]+$\\)"))
    (let ((name (match-string-no-properties 1))
	  (line (match-string-no-properties 3))
	  (text (match-string-no-properties 4)))
      (list (expand-file-name name default-directory)
	    (string-to-number line)
	    text))))

(defun wgrep-open-file (file)
  (if (file-exists-p file)
      (or (get-file-buffer file)
	  (find-file-noselect file))
    nil))

(defun wgrep-check-buffer ()
  "*check the file status. If it is possible to change file, return t"
  (cond
   ((or (null buffer-file-name)
	(not (file-exists-p buffer-file-name)))
    nil)
   (wgrep-change-readonly-file
    t)
   (buffer-read-only
    nil)
   (t t)))

(defun wgrep-apply-to-buffer (line new-text)
  "*The changes on the grep buffer apply to the file"
  (let ((inhibit-read-only wgrep-change-readonly-file))
    (goto-line line)
    (delete-region (line-beginning-position)
                   (line-end-position))
    (beginning-of-line)
    (insert new-text)))

(defun wgrep-put-color-file ()
  "*Highlight the changed line of the file"
  (let ((ov (make-overlay
                  (line-beginning-position)
                  (+ 1 (line-end-position)))))
    (overlay-put ov 'face 'wgrep-file-face)
    (overlay-put ov 'priority 0)
    (setq wgrep-file-overlays (cons ov wgrep-file-overlays))))

(defun wgrep-put-done-face ()
  (when (looking-at wgrep-line-file-regexp)
    (let ((ov (make-overlay (match-end 0) (+ 1 (line-end-position)))))
      (overlay-put ov 'face 'wgrep-done-face)
      (overlay-put ov 'priority 0)
      (setq wgrep-done-overlays (cons ov wgrep-done-overlays)))))

(defun wgrep-put-reject-face ()
  (when (looking-at wgrep-line-file-regexp)
    (let ((ov (make-overlay (match-end 0) (+ 1 (line-end-position)))))
      (overlay-put ov 'face 'wgrep-reject-face)
      (overlay-put ov 'priority 0)
      (setq wgrep-reject-overlays (cons ov wgrep-reject-overlays)))))

(defun wgrep-to-grep-mode ()
  (use-local-map grep-mode-map)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (setq buffer-read-only t))

(defun wgrep-finish-edit ()
  "Apply changed text to file buffers."
  (interactive)
  (save-excursion
    (while wgrep-overlays
      (let ((ov (car wgrep-overlays))
	    local-buf edited info)
	(setq wgrep-overlays (cdr wgrep-overlays))
	(goto-char (overlay-start ov))
	(when (setq info (wgrep-get-info))
	  (setq local-buf (wgrep-open-file (nth 0 info)))
	  (when local-buf
	    (with-current-buffer local-buf
	      (when (wgrep-check-buffer)
		(wgrep-apply-to-buffer (nth 1 info) (nth 2 info))
		(wgrep-put-color-file) ;; hilight the changed lines
		(setq edited t)))
	    (if edited
		(wgrep-put-done-face)
	      (wgrep-put-reject-face))))
	(delete-overlay ov))))
  (wgrep-to-grep-mode))

(defun wgrep-remove-change (beg end)
  "Remove color the region between BEG and END."
  (interactive "r")
  (let ((ovs (overlays-in beg end)))
    (while ovs
      (when (overlay-get (car ovs) 'wgrep)
	(delete-overlay (car ovs)))
      (setq ovs (cdr ovs))))
  (setq mark-active nil))

(defun wgrep-remove-all-change ()
  "Remove color whole buffer."
  (interactive)
  (wgrep-remove-change (point-min) (point-max)))

(defun wgrep-to-wgrep-mode ()
  "Prepare editing buffer."
  (interactive)
  (unless (eq major-mode 'grep-mode)
    (error "Not a grep buffer"))
  (unless (wgrep-process-exited-p)
    (error "Active process working"))
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (use-local-map wgrep-mode-map)
  (buffer-disable-undo)
  (wgrep-initialize-buffer)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (message "%s" (substitute-command-keys
		 "Press \\[wgrep-finish-edit] when finished")))

(defun wgrep-initialize-buffer ()
  (save-excursion
    (wgrep-goto-first-found)
    (let (after-change-functions buffer-read-only)
      (while (not (eobp))
	(cond
	 ((looking-at wgrep-line-file-regexp)
	  (let ((filename (match-string 1))
		(line (string-to-number (match-string 3))))
	    ;; delete backward, forward -A (--after-context) -B  (--before-context)
	    (save-excursion
	      (wgrep-prepare-context filename line nil))
	    (wgrep-prepare-context filename line t)
	    (forward-line -1)))
	 ((looking-at "^--$")
	  (wgrep-delete-region
	   (line-beginning-position)
	   (save-excursion (forward-line 1) (point)))
	  (forward-line -1)))
	(forward-line 1)))))

(defun wgrep-goto-first-found ()
  (goto-char (point-min))
  (while (and (not (eobp))
	      (not (get-text-property (point) 'face)))
    (forward-line 1)))

(defun wgrep-prepare-context (filename line forward)
  (let ((diff (if forward 1 -1))
	next)
    (setq next (+ diff line))
    (forward-line diff)
    (let ((inhibit-read-only t))
      (while (looking-at (format "^%s\\(-\\)%d\\(-\\)" filename next))
	(replace-match (format "%s:%d:" filename next) nil nil nil 0)
	;; -A -B output may be misunderstood and set read-only.
	;; (ex: filename-20-2010/01/01 23:59:99)
	;; To obey the properties order. '(read-only face) not works.
	(remove-text-properties (point) (line-end-position) 
				'(face read-only) (current-buffer))
	(forward-line diff)
	(setq next (+ diff next))))))

(defun wgrep-delete-region (min max)
  (let ((inhibit-read-only t))
    (remove-text-properties min max '(read-only) (current-buffer)))
  (delete-region min max))

(defun wgrep-process-exited-p ()
  (let ((proc (get-buffer-process (current-buffer))))
    (or (null proc)
	(eq (process-status proc) 'exit))))

(defun wgrep-finish-function (buffer msg)
  (when (with-current-buffer buffer
	  (wgrep-process-exited-p))
    (wgrep-set-readonly-area t)
    (add-hook 'after-change-functions 'wgrep-mode-change-face nil t)))

(provide 'wgrep)

;;; end
;;; wgrep.el ends here
