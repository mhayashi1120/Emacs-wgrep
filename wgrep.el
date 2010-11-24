;;; wgrep --- Writable grep buffer and apply the changes to files

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: grep edit extensions
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el
;; URL: http://www.emacswiki.org/emacs/download/wgrep.el
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

;; To avoid slowing down grep process, turn off the filter by
;;   M-x wgrep-disable-filter

;; To save all buffers that wgrep changed by
;;   M-x wgrep-save-all-buffers

;;; History:

;; This program is forked version. Original version can be downloaded from
;; http://www.bookshelf.jp/elc/grep-edit.el

;; Following added implementations and differences.
;; * Support GNU grep context option -A -B and -C
;; * Some bugfix. (wrong coloring text etc..)
;; * wdired.el like interface.
;; * Remove all advice.
;; * Bind to local variables. (grep-a-lot.el works well)
;; * After save buffer, colored face will be removed.
;; * Change face easy to see.

;;; TODO:

;; * can remove whole line. (In file buffer)

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

(defcustom wgrep-huge-buffer-size-threshold 524288
  "*Buffer size of wgrep considering as big buffer."
  :type 'integer
  :group 'wgrep)

(defvar wgrep-setup-hook nil
  "Hooks run when setup to wgrep.")

(defface wgrep-face
  '((((class color)
      (background dark))
     (:background "SlateGray1" :weight bold :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :weight bold :foreground "white"))
    (t
     ()))
  "*Face used for the changed text on grep buffer."
  :group 'wgrep)

(defface wgrep-file-face
  '((((class color)
      (background dark))
     (:background "gray30" :weight bold :foreground "white"))
    (((class color)
      (background light))
     (:background "ForestGreen" :weight bold :foreground "white"))
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

(defvar wgrep-each-other-buffer nil)
(make-variable-buffer-local 'wgrep-each-other-buffer)

(defvar wgrep-enabled-filter t)

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
  (if wgrep-enabled-filter
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
       (define-key grep-mode-map x 'wgrep-change-to-wgrep-mode))
     (where-is-internal 'wgrep-to-wgrep-mode grep-mode-map))
    (if (boundp 'compilation-finish-functions)
	(remove-hook 'compilation-finish-functions 'wgrep-finish-function t)
      (when (and (local-variable-p 'compilation-finish-function)
		 (eq compilation-finish-function 'wgrep-finish-function))
	(kill-local-variable 'compilation-finish-function)))
    (remove-hook 'compilation-filter-hook 'wgrep-grep-filter t))
  (remove-hook 'post-command-hook 'wgrep-maybe-echo-error-at-point t))

(defun wgrep-maybe-echo-error-at-point ()
  (when (null (current-message))
    (let ((o (find-if
	      (lambda (o)
		(overlay-get o 'wgrep-reject-message))
	      (overlays-in (line-beginning-position) (line-end-position)))))
      (when o
	(let (message-log-max)
	  (message "%s" (overlay-get o 'wgrep-reject-message)))))))

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

(defun wgrep-get-line-info ()
  (forward-line 0)
  (when (looking-at (concat wgrep-line-file-regexp "\\([^\n]*$\\)"))
    (let ((name (match-string-no-properties 1))
	  (line (match-string-no-properties 3))
	  (text (match-string-no-properties 4))
	  (start (match-beginning 4))
	  ov)
      (setq ov
	    (or
	     ;; get existing overlay
	     (find-if 
	      (lambda (o)
		(memq (overlay-get o 'face) '(wgrep-reject-face wgrep-done-face)))
	      (overlays-in start (line-end-position)))
	     (wgrep-make-overlay start (line-end-position))))
      (list (expand-file-name name default-directory)
	    (string-to-number line)
	    text
	    ov))))

(put 'wgrep-error 'error-conditions '(wgrep-error error))
(put 'wgrep-error 'error-message "Applying error.")

(defun wgrep-get-file-buffer (file)
  (unless (file-exists-p file)
    (signal 'wgrep-error "File is not exists."))
  (unless (file-writable-p file)
    (signal 'wgrep-error "File is not writable."))
  (or (get-file-buffer file)
      (find-file-noselect file)))

(defun wgrep-check-buffer ()
  "Check the file status. If it is possible to change file, return t"
  (when (and (not wgrep-change-readonly-file)
	     buffer-read-only)
    (signal 'wgrep-error (format "Buffer \"%s\" is read-only." (buffer-name)))))

;; not consider other edit. (ex: Undo or self-insert-command)
(defun wgrep-after-save-hook ()
  (remove-hook 'after-save-hook 'wgrep-after-save-hook t)
  (mapc
   (lambda (ov)
     (delete-overlay ov))
   wgrep-file-overlays)
  (kill-local-variable 'wgrep-file-overlays))

(defun wgrep-apply-to-buffer (buffer info old-text)
  "*The changes on the grep buffer apply to the file"
  (with-current-buffer buffer
    (let ((line (nth 1 info))
	  (new-text (nth 2 info))
	  (result (nth 3 info))
	  (inhibit-read-only wgrep-change-readonly-file))
      (wgrep-check-buffer)
      (save-restriction
	(widen)
	(wgrep-goto-line line)
	;;FIXME simply do this?
	(when (and (= line 1) 
		   buffer-file-coding-system
		   (coding-system-get buffer-file-coding-system :bom))
	  (setq old-text (wgrep-string-replace-bom old-text buffer-file-coding-system))
	  (setq new-text (wgrep-string-replace-bom new-text buffer-file-coding-system)))
	(unless (string= old-text
			 (buffer-substring (line-beginning-position) (line-end-position)))
	  (signal 'wgrep-error "Buffer was changed after grep."))
	(delete-region (line-beginning-position) (line-end-position))
	(forward-line 0)
	(insert new-text)
	;; hilight the changed line
	(wgrep-put-color-file)))))

;;Hack function
(defun wgrep-string-replace-bom (string cs)
  (let ((regexp (car (rassq (coding-system-base cs) auto-coding-regexp-alist)))
	str)
    ;;FIXME use find-operation-coding-system?
    (if (and regexp 
	     (setq str (encode-coding-string string (terminal-coding-system)))
	     (string-match regexp str))
	(substring str (match-end 0))
      string)))

(defun wgrep-put-color-file ()
  "*Highlight the changed line of the file"
  (let ((ov (wgrep-make-overlay
	     (line-beginning-position)
	     (line-end-position))))
    (overlay-put ov 'face 'wgrep-file-face)
    (overlay-put ov 'priority 0)
    (add-hook 'after-save-hook 'wgrep-after-save-hook nil t)
    (setq wgrep-file-overlays (cons ov wgrep-file-overlays))))

(defun wgrep-put-done-face (ov)
  (wgrep-set-face ov 'wgrep-done-face))

(defun wgrep-put-reject-face (ov message)
  (wgrep-set-face ov 'wgrep-reject-face message))

(defun wgrep-set-face (ov face &optional message)
  (overlay-put ov 'face face)
  (overlay-put ov 'priority 1)
  (overlay-put ov 'wgrep-reject-message message))

(defun wgrep-put-change-face (beg end)
  (save-excursion
    ;; looking-at destroy replace regexp..
    (save-match-data
      (forward-line 0)
      (let ((inhibit-it nil)
	    header value origin ovs ov)
	(when (looking-at wgrep-line-file-regexp)
	  ;; check file name point or not
	  (setq inhibit-it (> (match-end 0) beg))
	  (setq header (match-string-no-properties 0))
	  (setq value (buffer-substring-no-properties 
		       (match-end 0) (line-end-position)))
	  (unless inhibit-it
	    (setq ovs (overlays-in (line-beginning-position) (line-end-position)))
	    (while ovs
	      (when (overlay-get (car ovs) 'wgrep-changed)
		(when (string= (overlay-get (car ovs) 'wgrep-original-value) value)
		  (setq wgrep-overlays (remove (car ovs) wgrep-overlays))
		  (delete-overlay (car ovs)))
		(setq inhibit-it t))
	      (setq ovs (cdr ovs))))
	  (unless inhibit-it
	    (setq origin (wgrep-get-original-value header))
	    (setq ov (wgrep-make-overlay
		      (line-beginning-position)
		      (line-end-position)))
	    (overlay-put ov 'wgrep-changed t)
	    (overlay-put ov 'face 'wgrep-face)
	    (overlay-put ov 'priority 0)
	    (overlay-put ov 'wgrep-original-value origin)
	    (setq wgrep-overlays (cons ov wgrep-overlays))))))))

(defun wgrep-to-grep-mode ()
  (remove-hook 'after-change-functions 'wgrep-after-change-function t)
  (use-local-map grep-mode-map)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (setq buffer-read-only t))

(defun wgrep-changed-overlay-action (ov)
  (let (local-buf info)
    (if (eq (overlay-start ov) (overlay-end ov))
	;; ignore removed line or removed overlay
	t
      (goto-char (overlay-start ov))
      (cond
       ((null (setq info (wgrep-get-line-info)))
	;; ignore non grep result line.
	t)
       (t
	(let ((file (nth 0 info))
	      (result (nth 3 info)))
	  (condition-case err
	      (progn
		(wgrep-apply-to-buffer (wgrep-get-file-buffer file) info
				       (overlay-get ov 'wgrep-original-value))
		(wgrep-put-done-face result)
		t)
	    (wgrep-error
	     (wgrep-put-reject-face result (cdr err))
	     nil)
	    (error 
	     (wgrep-put-reject-face result (prin1-to-string err))
	     nil))))))))

(defun wgrep-finish-edit ()
  "Apply changed text to file buffers."
  (interactive)
  (let ((count 0))
    (save-excursion
      (let ((not-yet (copy-seq wgrep-overlays)))
	(while wgrep-overlays
	  (let ((ov (car wgrep-overlays)))
	    (setq wgrep-overlays (cdr wgrep-overlays))
	    (when (wgrep-changed-overlay-action ov)
	      (delete-overlay ov)
	      (setq not-yet (delq ov not-yet))
	      (incf count))))
	;; restore overlays
	(setq wgrep-overlays not-yet)))
    (wgrep-cleanup-temp-buffer)
    (wgrep-to-grep-mode)
    (let ((msg (format "(%d changed)" count)))
      (cond
       ((null wgrep-overlays)
	(if (= count 0)
	    (message "(No changes to be performed)")
	  (message "Successfully finished. %s" msg)))
       ((= (length wgrep-overlays) 1)
	(message "There is unapplied change. %s" msg))
       (t
	(message "There are %d unapplied changes. %s" 
		 (length wgrep-overlays) msg))))))

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
  (wgrep-restore-from-temp-buffer)
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
  (wgrep-clone-to-temp-buffer)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (set-buffer-modified-p wgrep-overlays) ;; restore modified status
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
    (when (wgrep-buffer-huge-p)
      (unless (y-or-n-p "Buffer is too large. Ready to freeze several minutes? ")
	(error "Buffer is too large.")))
    (wgrep-set-readonly-area (not wgrep-readonly-state))
    (set-buffer-modified-p modified)
    (if wgrep-readonly-state
	(message "Now **disable** to remove whole line.")
      (message "Now enable to remove whole line."))))

(defun wgrep-disable-filter ()
  "Disable filter."
  (interactive)
  (setq wgrep-enabled-filter nil)
  (message "Wgrep is **disabled**."))

(defun wgrep-enable-filter ()
  "Enable filter."
  (interactive)
  (setq wgrep-enabled-filter t)
  (message "Wgrep is enabled."))

(defun wgrep-toggle-filter ()
  "Toggle wgrep filter turn on/off.
You can use \\[wgrep-change-to-wgrep-mode] when disabled.
"
  (interactive)
  (if wgrep-enabled-filter
      (wgrep-disable-filter)
    (wgrep-enable-filter)))

(defun wgrep-change-to-wgrep-mode ()
  "Change to wgrep mode. Useful when wgrep filter was disabled.

When huge *grep* buffer, freezing several minutes.
"
  (interactive)
  (unless (eq major-mode 'grep-mode)
    (error "Not a grep buffer"))
  (let ((huge (wgrep-buffer-huge-p)))
    (when (or (not huge)
	      (y-or-n-p "Buffer is too large. Ready to freeze several minutes? "))
      (setq huge nil)
      (save-excursion
	(goto-char (point-max))
	(wgrep-grep-filter)))
    (wgrep-finish-function (current-buffer) nil)
    (when huge
      (setq wgrep-readonly-state nil))
    (wgrep-to-wgrep-mode)
    (when huge
      ;; Huge buffer is rarely created.
      ;; Ignore current message.
      (message "Header is writable. Be carefull to edit!"))))

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
	    ;; delete backward and forward following options.
	    ;; -A (--after-context) -B  (--before-context) -C (--context)
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

(defun wgrep-goto-end-of-found ()
  (goto-char (point-max))
  (while (and (not (bobp))
	      (not (get-text-property (point) 'face)))
    (forward-line -1))
  (forward-line 1))

(defun wgrep-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun wgrep-prepare-context (filename line forward)
  (let ((diff (if forward 1 -1))
	next line-head)
    (setq next (+ diff line))
    (forward-line diff)
    (let ((inhibit-read-only t))
      (while (looking-at (format "^%s\\(-\\)%d\\(-\\)" filename next))
	(setq line-head (format "%s:%d:" filename next))
	(wgrep-set-readonly-property 0 (length line-head) line-head)
	(replace-match line-head nil nil nil 0)
	;; -A -B -C output may be misunderstood and set read-only.
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
      ;; backward to previous read-only
      (while (and (re-search-backward regexp nil t)
		  (not (get-text-property (point) 'read-only)))
	(wgrep-set-readonly-property (match-beginning 0)
				     (match-end 0))))))

(defun wgrep-set-readonly-property (start end &optional object)
  (set-text-properties start end '(read-only t) object)
  ;; This means grep header (filename and line num) that rear is editable text.
  ;; Header text length will always be greater than 2.
  (when (> end (1+ start))
    (add-text-properties (1- end) end '(rear-nonsticky t) object)))

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
	(wgrep-goto-end-of-found)
	(setq beg (point))
	(setq end (point-max))
	(when beg
	  (put-text-property beg end 'read-only t))))
    (add-hook 'post-command-hook 'wgrep-maybe-echo-error-at-point nil t)
    (setq wgrep-readonly-state t)))

(defun wgrep-make-overlay (beg end)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'wgrep t)
    o))

(defun wgrep-clone-to-temp-buffer ()
  (wgrep-cleanup-temp-buffer)
  (let ((grepbuf (current-buffer))
	(tmpbuf (generate-new-buffer " *wgrep temp* ")))
    (setq wgrep-each-other-buffer tmpbuf)
    (add-hook 'kill-buffer-hook 'wgrep-cleanup-temp-buffer nil t)
    (append-to-buffer tmpbuf (point-min) (point-max))
    (with-current-buffer tmpbuf
      (setq wgrep-each-other-buffer grepbuf))
    tmpbuf))

(defun wgrep-restore-from-temp-buffer ()
  (cond
   ((and wgrep-each-other-buffer
	 (buffer-live-p wgrep-each-other-buffer))
    (let ((grepbuf (current-buffer))
	  (tmpbuf wgrep-each-other-buffer)
	  (savedh (wgrep-current-header))
	  (savedc (current-column))
	  (savedp (point))
	  (inhibit-read-only t)
	  buffer-read-only)
      (erase-buffer)
      (with-current-buffer tmpbuf
	(append-to-buffer grepbuf (point-min) (point-max)))
      (goto-char (point-min))
      (or (and savedh
	       (re-search-forward (concat "^" (regexp-quote savedh)) nil t)
	       (move-to-column savedc))
	  (goto-char (min (point-max) savedp)))
      (wgrep-cleanup-temp-buffer)
      (setq wgrep-overlays nil)))
   (t
    ;; non fatal error
    (message "Error! Saved buffer is unavailable."))))

(defun wgrep-cleanup-temp-buffer ()
  "Cleanup temp buffer in *grep* buffer."
  (when (memq major-mode '(grep-mode))
    (let ((grep-buffer (current-buffer)))
      (mapc
       (lambda (buf)
	 (with-current-buffer buf
	   (when (eq grep-buffer wgrep-each-other-buffer)
	     (kill-buffer buf))))
       (buffer-list)))
    (setq wgrep-each-other-buffer nil)))

(defun wgrep-buffer-huge-p ()
  (and wgrep-huge-buffer-size-threshold 
       (> (buffer-size) wgrep-huge-buffer-size-threshold)))

(defun wgrep-current-header ()
  (save-excursion
    (forward-line 0)
    (when (looking-at wgrep-line-file-regexp)
      (match-string-no-properties 0))))

(defun wgrep-get-original-value (header)
  (when (and wgrep-each-other-buffer
	     (buffer-live-p wgrep-each-other-buffer))
    (with-current-buffer wgrep-each-other-buffer
      (goto-char (point-min))
      (when (re-search-forward (concat "^" (regexp-quote header)) nil t)
	(buffer-substring-no-properties (point) (line-end-position))))))

(provide 'wgrep)

;;; wgrep.el ends here
