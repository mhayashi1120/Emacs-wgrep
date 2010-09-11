;;; wgrep --- Writable grep buffer and apply the changes to files

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: grep edit extensions
;; URL: http://github.com/mhayashi1120/Emacs-Lisp/raw/master/wgrep.el
;; URL: http://www.emacswiki.org/download/wgrep.el
;; Emacs: GNU Emacs 22 or later

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
;; the file buffer.

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'wgrep)

;;; Usage:

;; You can edit the text on *grep* buffer after type C-c C-p.
;; After that the changed text is highlighted.
;; Following keybind is defined.

;; C-c C-e : Apply the highlighting changes to file buffers.
;; C-c C-u : All changes are unmarked and ignored.
;; C-c C-r : Remove the highlight in the region (The Changes doesn't
;;      apply to files. Of course, if you type C-c C-e, the remained
;;      highlight changes are applied to files.)
;; C-c C-p Toggle read-only area.
;; C-c C-k Discard all changes and exit.
;; C-x C-q Exit wgrep mode.

;; To turn on/off the feature by
;;   M-x wgrep-toggle-feature

;; To save all buffers that wgrep changed by
;;   M-x wgrep-save-all-buffers

;;; History:

;; This program is forked version. Original version can be downloaded from
;; http://www.bookshelf.jp/elc/grep-edit.el

;; Following added implementations and differences.
;; * Support grep option -A (--after-context) -B (--before-context)
;; * Some bugfix. (wrong coloring text etc..)
;; * wdired.el like interface.
;; * Remove all advice.
;; * Bind to local variables. (grep-a-lot.el works well)
;; * After save buffer, colored face will be removed.

;;; TODO:

;; * can undo region.
;; * can remove whole line.
;; * When applying buffer is modified.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'grep)

(defgroup wgrep nil
  "Customize wgrep"
  :group 'grep)

(defcustom wgrep-change-readonly-file nil
  "*Non-nil means to change read only files."
  :group 'wgrep
  :type 'boolean)

(defcustom wgrep-enable-key "\C-c\C-p"
  "*Key to enable `wgrep-mode'."
  :type 'string  
  :group 'wgrep)

(defvar wgrep-setup-hook nil
  "Hooks run when setup to wgrep.")

(defface wgrep-face
  '((((class color)
      (background dark))
     (:background "SlateGray1" :weight bold :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :weight bold))
    (t
     ()))
  "*Face used for the changed text on grep buffer."
  :group 'wgrep)

(defface wgrep-file-face
  '((((class color)
      (background dark))
     (:background "gray30" :weight bold))
    (((class color)
      (background light))
     (:background "ForestGreen" :weight bold))
    (t
     ()))
  "*Face used for the changed text on file buffer."
  :group 'wgrep)

(defface wgrep-reject-face
  '((((class color)
      (background dark))
     (:foreground "hot pink" :weight bold))
    (((class color)
      (background light))
     (:foreground "red" :weight bold))
    (t
     ()))
  "*Face used for the line on grep buffer that can not apply to file."
  :group 'wgrep)

(defface wgrep-done-face
  '((((class color)
      (background dark))
     (:foreground "LightSkyBlue" :weight bold))
    (((class color)
      (background light))
     (:foreground "blue" :weight bold))
    (t
     ()))
  "*Face used for the line on grep buffer that can apply to file."
  :group 'wgrep)

(defvar wgrep-overlays nil)
(make-variable-buffer-local 'wgrep-overlays)

(defvar wgrep-file-overlays nil)
(make-variable-buffer-local 'wgrep-file-overlays)

(defvar wgrep-readonly-state nil)
(make-variable-buffer-local 'wgrep-readonly-state)

(defvar wgrep-enabled t)

(defconst wgrep-line-file-regexp (caar grep-regexp-alist))

(add-hook 'grep-setup-hook 'wgrep-setup)

(defvar wgrep-mode-map nil)
(unless wgrep-mode-map
  (setq wgrep-mode-map
	(let ((map (make-sparse-keymap)))

	  (define-key map "\C-c\C-c" 'wgrep-finish-edit)
	  (define-key map "\C-c\C-e" 'wgrep-finish-edit)
	  (define-key map "\C-c\C-p" 'wgrep-toggle-readonly-area)
	  (define-key map "\C-c\C-r" 'wgrep-remove-change)
	  (define-key map "\C-x\C-s" 'wgrep-finish-edit)
	  (define-key map "\C-c\C-u" 'wgrep-remove-all-change)
	  (define-key map "\C-c\C-[" 'wgrep-remove-all-change)
	  (define-key map "\C-c\C-k" 'wgrep-abort-changes)
	  (define-key map "\C-x\C-q" 'wgrep-exit)
	  (define-key map "\C-m"     'ignore)
	  (define-key map "\C-j"     'ignore)
	  (define-key map "\C-o"     'ignore)

	  map)))

(defun wgrep-setup ()
  (if wgrep-enabled
      (progn
	(define-key grep-mode-map wgrep-enable-key 'wgrep-to-wgrep-mode)
	(if (boundp 'compilation-finish-functions)
	    (add-hook 'compilation-finish-functions 'wgrep-finish-function nil t)
	  ;; this works Emacs 22.1 or earlier
	  (set (make-local-variable 'compilation-finish-function) 'wgrep-finish-function))
	(add-hook 'compilation-filter-hook 'wgrep-grep-filter nil t)
	(run-hooks 'wgrep-setup-hook))
    (mapc
     (lambda (x)
       (define-key grep-mode-map x 'ignore))
     (where-is-internal 'wgrep-to-wgrep-mode grep-mode-map))
    (if (boundp 'compilation-finish-functions)
	(remove-hook 'compilation-finish-functions 'wgrep-finish-function t)
      (when (and (local-variable-p 'compilation-finish-function)
		 (eq compilation-finish-function 'wgrep-finish-function))
	(kill-local-variable 'compilation-finish-function)))
    (remove-hook 'compilation-filter-hook 'wgrep-grep-filter t)))

(defun wgrep-set-readonly-area (state)
  (let ((inhibit-read-only t)
	(regexp (format "\\(?:%s\\|\n\\)" wgrep-line-file-regexp))
	beg end)
    (save-excursion
      (wgrep-goto-first-found)
      (while (re-search-forward regexp nil t)
        (put-text-property (match-beginning 0)
                           (match-end 0) 'read-only state)))
    (setq wgrep-readonly-state state)))

(defun wgrep-after-change-function (beg end leng-before)
  (when (wgrep-process-exited-p)
    (cond
     ((= (point-min) (point-max))
      ;; cleanup when first executing
      (mapc
       (lambda (o)
	 (delete-overlay o))
       (remove-if-not 
	(lambda (o) (overlay-get o 'wgrep))
	(overlays-in (point-min) (point-max)))))
     (t
      (wgrep-put-change-face beg end)))))

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

;; not consider other edit. (ex: Undo or self-insert-command)
(defun wgrep-after-save-hook ()
  (remove-hook 'after-save-hook 'wgrep-after-save-hook t)
  (mapc
   (lambda (ov)
     (delete-overlay ov))
   wgrep-file-overlays)
  (kill-local-variable 'wgrep-file-overlays))

(defun wgrep-apply-to-buffer (line new-text)
  "*The changes on the grep buffer apply to the file"
  (let ((inhibit-read-only wgrep-change-readonly-file))
    (wgrep-goto-line line)
    (delete-region (line-beginning-position)
                   (line-end-position))
    (beginning-of-line)
    (insert new-text)))

(defun wgrep-put-color-file ()
  "*Highlight the changed line of the file"
  (let ((ov (wgrep-make-overlay
	     (line-beginning-position)
	     (line-end-position))))
    (overlay-put ov 'face 'wgrep-file-face)
    (overlay-put ov 'priority 0)
    (add-hook 'after-save-hook 'wgrep-after-save-hook nil t)
    (setq wgrep-file-overlays (cons ov wgrep-file-overlays))))

(defun wgrep-put-done-face ()
  (when (looking-at wgrep-line-file-regexp)
    (let ((ov (wgrep-make-overlay (match-end 0) (line-end-position))))
      (overlay-put ov 'face 'wgrep-done-face)
      (overlay-put ov 'priority 0))))

(defun wgrep-put-reject-face ()
  (when (looking-at wgrep-line-file-regexp)
    (let ((ov (wgrep-make-overlay (match-end 0) (line-end-position))))
      (overlay-put ov 'face 'wgrep-reject-face)
      (overlay-put ov 'priority 0))))

(defun wgrep-put-change-face (beg end)
  (let ((ovs (overlays-in beg end))
	(inhibit-it nil)
	ov)
    (save-excursion
      (forward-line 0)
      (when (looking-at wgrep-line-file-regexp)
	(setq inhibit-it (> (match-end 0) beg))))
    (unless inhibit-it
      (while ovs
	(if (overlay-get (car ovs) 'wgrep)
	    (setq inhibit-it t))
	(setq ovs (cdr ovs))))
    (unless inhibit-it
      (setq ov (wgrep-make-overlay
		(line-beginning-position)
		(line-end-position)))
      (overlay-put ov 'face 'wgrep-face)
      (overlay-put ov 'priority 0)
      (setq wgrep-overlays (cons ov wgrep-overlays)))))

(defun wgrep-to-grep-mode ()
  (remove-hook 'after-change-functions 'wgrep-after-change-function t)
  (use-local-map grep-mode-map)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (setq buffer-read-only t))

(defun wgrep-finish-edit ()
  "Apply changed text to file buffers."
  (interactive)
  (save-excursion
    (let (not-yet-overlays)
      (while wgrep-overlays
	(let ((ov (car wgrep-overlays))
	      local-buf done info)
	  (setq wgrep-overlays (cdr wgrep-overlays))
	  (if (eq (overlay-start ov) (overlay-end ov))
	      ;; ignore removed line or removed overlay
	      (setq done t)
	    (goto-char (overlay-start ov))
	    (when (setq info (wgrep-get-info))
	      (setq local-buf (wgrep-open-file (nth 0 info)))
	      (when local-buf
		(with-current-buffer local-buf
		  (when (wgrep-check-buffer)
		    (wgrep-apply-to-buffer (nth 1 info) (nth 2 info))
		    (wgrep-put-color-file) ;; hilight the changed line
		    (setq done t))))
	      (if done
		  (wgrep-put-done-face)
		(wgrep-put-reject-face))))
	  (if done
	      (delete-overlay ov)
	    (setq not-yet-overlays (cons ov not-yet-overlays)))))
      ;; restore overlays
      (setq wgrep-overlays not-yet-overlays)))
  (wgrep-to-grep-mode)
  (cond
   ((null wgrep-overlays)
    (message "Successfully finished."))
   ((= (length wgrep-overlays) 1)
    (message "There is unapplied change."))
   (t
    (message "There are %d unapplied changes." (length wgrep-overlays)))))

(defun wgrep-exit ()
  "Return to `grep-mode'"
  (interactive)
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "Buffer %s modified; save changes? "
			     (current-buffer))))
      (wgrep-finish-edit)
    (wgrep-abort-changes)))

(defun wgrep-abort-changes ()
  "Discard all changes and return to `grep-mode'"
  (interactive)
  (wgrep-remove-all-change)
  (wgrep-to-grep-mode)
  (message "Changes aborted"))

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
  (add-hook 'after-change-functions 'wgrep-after-change-function nil t)
  (use-local-map wgrep-mode-map)
  (buffer-disable-undo)
  (wgrep-initialize-buffer)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (message "%s" (substitute-command-keys
		 "Press \\[wgrep-finish-edit] when finished \
or \\[wgrep-abort-changes] to abort changes.")))

(defun wgrep-toggle-readonly-area ()
  "Toggle read-only area to remove whole line.

See the following example, you obviously don't want to edit first line.
If grep hit a lot of line, hard to edit the buffer.
After toggle to editable, you can call 
`delete-matching-lines', `delete-non-matching-lines'.

Example:
----------------------------------------------
./.svn/text-base/some.el.svn-base:87:(hoge)
./some.el:87:(hoge)
----------------------------------------------
"
  (interactive)
  (let ((modified (buffer-modified-p))
	after-change-functions)
    (wgrep-set-readonly-area (not wgrep-readonly-state))
    (set-buffer-modified-p modified)
    (if wgrep-readonly-state
	(message "Now **disable** to remove whole line.")
      (message "Now enable to remove whole line."))))

(defun wgrep-toggle-feature ()
  (interactive)
  (if (setq wgrep-enabled (not wgrep-enabled))
      (message "Wgrep is enabled.")
    (message "Wgrep is **disabled**.")))

(defun wgrep-save-all-buffers ()
  "Save buffers wgrep changed."
  (interactive)
  (let ((count 0))
    (mapc
     (lambda (b)
       (with-current-buffer b
	 (when (and (local-variable-p 'wgrep-file-overlays)
		    wgrep-file-overlays
		    (buffer-modified-p))
	   (basic-save-buffer)
	   (incf count))))
     (buffer-list))
    (cond
     ((= count 0)
      (message "No buffer is saved."))
     ((= count 1)
      (message "Buffer is saved."))
     (t
      (message "%d Buffers are saved." count)))))

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

(defun wgrep-goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun wgrep-prepare-context (filename line forward)
  (let ((diff (if forward 1 -1))
	next line-head)
    (setq next (+ diff line))
    (forward-line diff)
    (let ((inhibit-read-only t))
      (while (looking-at (format "^%s\\(-\\)%d\\(-\\)" filename next))
	(setq line-head (format "%s:%d:" filename next))
	(set-text-properties 0 (length line-head)
			     '(read-only t rear-nonsticky t) line-head)
	(replace-match line-head nil nil nil 0)
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

(defun wgrep-grep-filter ()
  "Set text read-only backward."
  (save-excursion
    (let ((inhibit-read-only t)
	  (regexp (format "\\(?:%s\\|\n\\)" wgrep-line-file-regexp)))
      (while (and (re-search-backward regexp nil t)
		  (not (get-text-property (point) 'read-only)))
	(set-text-properties (match-beginning 0)
			     (match-end 0) '(read-only t rear-nonsticky t))))))

(defun wgrep-finish-function (buffer msg)
  (when (with-current-buffer buffer
	  (wgrep-process-exited-p))
    (save-excursion
      (let ((inhibit-read-only t)
	    buffer-read-only
	    beg end)
	;; Grep result header
	(setq beg (point-min))
	(wgrep-goto-first-found)
	(setq end (point))
	(put-text-property beg end 'read-only t)
	;; Grep result footer
	(setq beg (previous-single-property-change (point-max) 'read-only))
	(setq end (point-max))
	(when beg
	  (put-text-property beg end 'read-only t))))
    (setq wgrep-readonly-state t)))

(defun wgrep-make-overlay (beg end)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'wgrep t)
    o))

(provide 'wgrep)

;;; wgrep.el ends here
