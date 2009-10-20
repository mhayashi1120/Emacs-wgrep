;;; grep-edit --- edit grep buffer and apply the changes to files
;; -*- Mode: Emacs-Lisp -*-

;;  $Id: grep-edit.el,v 2.11 2009-10-20 12:12:55 akihisa Exp $

;; Author: Matsushita Akihisa <akihisa@mail.ne.jp>
;; Keywords: grep edit

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

;; grep-edit provides to edit grep buffer and to apply the changes to
;; the file.
;;

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'grep-edit)

;; The latest version of this program can be downloaded from
;; http://www.bookshelf.jp/elc/grep-edit.el

;; related my elisp
;; http://www.bookshelf.jp/elc/color-grep.el
;; http://www.bookshelf.jp/elc/color-moccur.el
;; http://www.bookshelf.jp/elc/moccur-edit.el

;; Usage:
;; You can edit the text on *grep* buffer, and the changed text is
;; highlighted. Then, type C-c C-e to apply the highlighting changes
;; to files.

;; C-c C-e : apply the highlighting changes to files.
;; C-c C-u : All changes are ignored
;; C-c C-r : Remove the highlight in the region (The Changes doesn't
;; apply to files. Of course, if you type C-c C-e, the remained
;; highlight changes are applied to files.)

;;; History:

;; grep-edit 1.0 was released to the net on 12/03/2002
;;; Code:

(defgroup grep-edit nil
  "Customize grep-edit"
  :group 'grep)

(defcustom grep-edit-change-readonly-file nil
  "*Non-nil means to change read only files."
  :group 'grep-edit
  :type 'boolean)

(defface grep-edit-face
  '((((class color)
      (background dark))
     (:background "SlateGray1" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "*Face used for the changed text on grep buffer."
  :group 'grep-edit)

(defface grep-edit-file-face
  '((((class color)
      (background dark))
     (:background "gray30" :bold t))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "*Face used for the changed text on file buffer."
  :group 'grep-edit)

(defface grep-edit-reject-face
  '((((class color)
      (background dark))
     (:foreground "hot pink" :bold t))
    (((class color)
      (background light))
     (:foreground "red" :bold t))
    (t
     ()))
  "*Face used for the line on grep buffer that can not apply to file."
  :group 'grep-edit)

(defface grep-edit-done-face
  '((((class color)
      (background dark))
     (:foreground "LightSkyBlue" :bold t))
    (((class color)
      (background light))
     (:foreground "blue" :bold t))
    (t
     ()))
  "*Face used for the line on grep buffer that can apply to file."
  :group 'grep-edit)

(defvar grep-edit-overlays nil)
(defvar grep-edit-file-overlays nil)
(defvar grep-edit-reject-overlays nil)
(defvar grep-edit-done-overlays nil)
(make-local-variable 'grep-edit-file-overlays)
(defvar grep-edit-change-face-flg nil)

(add-hook 'grep-setup-hook
          (lambda ()
            (define-key grep-mode-map "\M-r" 'grep-narrow-down)
            (define-key grep-mode-map " "
              'self-insert-command)
            (define-key grep-mode-map [backspace]
              'backward-delete-char-untabify)
            (define-key grep-mode-map "\C-c\C-r"
              'grep-edit-remove-change)
            (define-key grep-mode-map "\C-c\C-e"
              'grep-edit-finish-edit)
            (define-key grep-mode-map "\C-c\C-u"
              'grep-edit-remove-all-change)

            (add-hook 'after-change-functions 'grep-mode-change-face nil t)
            ;;(remove-hook 'after-change-functions 'grep-mode-change-face)
            (setq grep-edit-change-face-flg nil)))

(defadvice compilation-handle-exit
  (after grep-edit-change-mode activate)
  (if (and
       (buffer-name (current-buffer))
       (string-match
        "grep"
        (buffer-name (current-buffer)))
       (string= process-status "run"))
      (setq grep-edit-change-face-flg nil)
    (if (and
         (buffer-name (current-buffer))
         (string-match
          "grep"
          (buffer-name (current-buffer)))
         (string= process-status "exit"))
        (progn
          (toggle-read-only)
          (buffer-enable-undo (current-buffer))
          (grep-edit-set-readonly-area t)
          (setq grep-edit-change-face-flg t)))))

(defun grep-edit-set-readonly-area (state)
  (let ((inhibit-read-only t) beg end)
    (save-excursion
      (goto-char (point-min))
      (setq beg (point))
      (forward-line 1)
      (end-of-line)
      (setq end (point))
      (put-text-property beg end 'read-only state)
      (while (re-search-forward "\\(^[^\n]+:[0-9]+\\)" nil t)
        (put-text-property (match-beginning 1)
                           (match-end 1) 'read-only state))
      (goto-char (point-min))
      (while (re-search-forward "\\(\n\\)" nil t)
        (put-text-property (match-beginning 1)
                           (match-end 1) 'read-only state)))))

(defun grep-mode-change-face (beg end leng-before)
  (let ((ov (overlays-in beg end))
        (exist-ovelays nil))
    (if (and grep-edit-change-face-flg
             (string= (buffer-name compilation-last-buffer)
                      (buffer-name (current-buffer))))
        (progn
          (while ov
            (if (overlay-get (car ov) 'grep-edit)
                (setq exist-ovelays t))
            (setq ov (cdr ov)))
          (if exist-ovelays
              ()
            (setq ov
                  (make-overlay
                   (line-beginning-position)
                   (+ 1 (line-end-position))))
            (overlay-put ov 'grep-edit t)
            (overlay-put ov 'face 'grep-edit-face)
            (overlay-put ov 'priority 0)
            (setq grep-edit-overlays (cons ov grep-edit-overlays)))))))

(defvar grep-edit-filename "")
(defvar grep-edit-line "")
(defvar grep-edit-text "")

(defadvice compile-goto-error
  (around grep-edit-color-stop activate)
  (let ((grep-edit-change-face-flg nil))
    ad-do-it))

(defadvice next-error
  (around grep-edit-color-stop-next activate)
  (let ((grep-edit-change-face-flg nil))
    ad-do-it))

(defun grep-edit-get-info ()
  (beginning-of-line)
  (if (re-search-forward
       "^\\([^\n()]+\\):\\([0-9]+\\):\\([^\n]+$\\)" nil t)
      (progn
        (setq grep-edit-filename
              (expand-file-name
               (buffer-substring-no-properties
                (match-beginning 1)
                (match-end 1))
               default-directory))
        (setq grep-edit-line
              (string-to-int
               (buffer-substring-no-properties
                (match-beginning 2)
                (match-end 2))))
        (setq grep-edit-text (buffer-substring-no-properties
                              (match-beginning 3)
                              (match-end 3))))))

(defun grep-edit-open-file ()
  (if (file-exists-p grep-edit-filename)
      (if (get-file-buffer (expand-file-name grep-edit-filename))
          (get-file-buffer (expand-file-name grep-edit-filename))
        (find-file-noselect grep-edit-filename))
    nil))

(defun grep-edit-check-file ()
  "*check the file status. If it is impossible to change file, return t"
  (cond
   (buffer-read-only
    nil)
   ((not (file-exists-p grep-edit-filename))
    nil)
   (t t)))

(defun grep-edit-change-file ()
  "*The changes on the grep buffer apply to the file"
  (let ((inhibit-read-only grep-edit-change-readonly-file))
    (goto-line grep-edit-line)
    (beginning-of-line)
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert grep-edit-text)))

(defun grep-edit-put-color-file ()
  "*Highlight the changed line of the file"
  (let ((ov))
    (setq fileov (make-overlay
                  (line-beginning-position)
                  (+ 1 (line-end-position))))
    (overlay-put fileov 'face 'grep-edit-file-face)
    (overlay-put fileov 'priority 0)
    (setq grep-edit-file-overlays (cons fileov grep-edit-file-overlays))))

(defun grep-edit-put-done-face ()
  (let ((ov))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "^[^\n]+:[0-9]+:" nil t)
      (setq ov (make-overlay (point) (+ 1 (line-end-position))))
      (overlay-put ov 'moccur-edit t)
      (overlay-put ov 'face 'grep-edit-done-face)
      (overlay-put ov 'priority 0)
      (setq grep-edit-done-overlays (cons ov grep-edit-done-overlays)))))

(defun grep-edit-put-reject-face ()
  (let ((ov))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "^[^\n]+:[0-9]+:" nil t)
      (setq ov (make-overlay (point) (+ 1 (line-end-position))))
      (overlay-put ov 'moccur-edit t)
      (overlay-put ov 'face 'grep-edit-reject-face)
      (overlay-put ov 'priority 0)
      (setq grep-edit-reject-overlays (cons ov grep-edit-reject-overlays)))))

(defun grep-edit-finish-edit ()
  (interactive)
  (let ((ov) fileov beg filename text line local-buf cbuf line)
    (setq cbuf (current-buffer))
    (while grep-edit-overlays
      (setq ov (car grep-edit-overlays))
      (setq grep-edit-overlays (cdr grep-edit-overlays))
      (setq beg (overlay-start ov))
      (if beg
          (progn
            (goto-char beg)
            (grep-edit-get-info)
            (setq local-buf (grep-edit-open-file))
            (if local-buf
                (set-buffer local-buf))

            (if (grep-edit-check-file) ;; check file
                (progn                 ;; if it can be edited
                  (grep-edit-change-file) ;; Change file
                  (grep-edit-put-color-file) ;; hilight the changed lines
                  (set-buffer cbuf)
                  (grep-edit-put-done-face))
              (progn
                (set-buffer cbuf)
                (grep-edit-put-reject-face)))
            ;; Return previous buffer
            (set-buffer cbuf)
            (delete-overlay ov))))))

(defun grep-edit-remove-change (beg end)
  (interactive "r")
  (let ((ov (overlays-in beg end)))
    (while ov
      (if (overlay-get (car ov) 'grep-edit)
          (delete-overlay (car ov)))
      (setq ov (cdr ov))))
  (setq mark-active nil))

(defun grep-edit-remove-all-change ()
  (interactive)
  (grep-edit-remove-change (point-min) (point-max)))

;; advice for query-replace
(defun grep-edit-add-skip-in-replace (command)
  "Advice COMMAND to skip matches while they have read-only properties.
This is useful to avoid \"read-only\" errors in search and replace
commands.  This advice only has effect in grep-edit mode."
  (eval
   `(defadvice ,command (around grep-edit-discard-read-only activate)
      ,(format "Make %s to work better with grep-edit,\n%s."  command
               "skipping read-only matches when invoked without argument")
      ad-do-it
      (if (eq major-mode 'grep-mode)
          (while (and ad-return-value
                      (text-property-any
                       (max 1 (1- (match-beginning 0))) (match-end 0)
                       'read-only t))
            ad-do-it))
      ad-return-value)))

(defun grep-edit-replace-advice (command)
  "Advice COMMAND to skip matches while they have read-only properties.
This is useful to avoid \"read-only\" errors in search and replace
commands.  This advice only has effect in grep-edit mode."
  (eval
   `(defadvice ,command (around grep-edit-grok-read-only activate)
      ,(format "Make %s to work better with grep-edit,\n%s."  command
               "skipping read-only matches when invoked without argument")
      (if (eq major-mode 'grep-mode)
          (progn
            (grep-edit-add-skip-in-replace 'search-forward)
            (grep-edit-add-skip-in-replace 're-search-forward)
            (unwind-protect
                ad-do-it
              (progn
                (ad-remove-advice 'search-forward
                                  'around 'grep-edit-discard-read-only)
                (ad-remove-advice 're-search-forward
                                  'around 'grep-edit-discard-read-only)
                (ad-update 'search-forward)
                (ad-update 're-search-forward))))
        ad-do-it)
      ad-return-value)))

(mapcar 'grep-edit-replace-advice
        '(query-replace query-replace-regexp replace-string))

(defadvice compile-internal
  (around inhibit-read-only-before-grep activate)
  (let ((mode (ad-get-arg 2))
        (default-directory default-directory))
    (when (and mode
               (string-match "grep" mode)
               (buffer-live-p
                (get-buffer
                 (concat "*" (downcase mode) "*"))))
      (save-current-buffer
        (kill-buffer (concat "*" (downcase mode) "*"))))
    ad-do-it))

(provide 'grep-edit)
;;; end
;;; grep-edit.el ends here
