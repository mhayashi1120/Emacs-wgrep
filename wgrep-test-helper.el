;;; wgrep-test-helper.el --- A wgrep test helper -*- lexical-binding: t-*-

;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-test-helper.el

(require 'wgrep)

(defun wgrep-test-helper--wait (buf)
  (let ((proc (get-buffer-process buf)))
    (while (eq (process-status proc) 'run)
      (sit-for 0.1))
    (sleep-for 0.2)
    (switch-to-buffer buf)))

(defun wgrep-test-helper--grep (command)
  (let ((buf (grep command)))
    (wgrep-test-helper--wait buf)))

(defun wgrep-test-helper--get-contents (file &optional cs)
  (let ((coding-system-for-read cs))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun wgrep-test-helper--prepare-file (file contents &optional cs)
  ;; cleanup for convenience
  (let ((buf (get-file-buffer file)))
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (let ((coding-system-for-write cs))
    (write-region contents nil file)))

(defun wgrep-test-helper--cleanup-file (file)
  (when (file-exists-p file)
    (delete-file file))
  (when (file-exists-p (concat file "~"))
    (delete-file (concat file "~"))))

(defmacro wgrep-test-helper--default (&rest body)
  `(let ((wgrep-change-readonly-file nil)
         (wgrep-auto-save-buffer nil))
     (progn ,@body)))

(defun wgrep-test-helper--ag (string file)
  (let ((buf (ag/search string default-directory :file-regex (regexp-quote file) :regexp t)))
    (wgrep-test-helper--wait buf)))

(defun wgrep-test-helper--deadgrep (string)
  (let ((deadgrep-project-root-function (lambda () default-directory))
        (deadgrep--search-type 'regexp))
    (deadgrep string))
  (wgrep-test-helper--wait (current-buffer)))

(defun wgrep-test-helper-fixture (data body-fn)
  (let ((test-directory (expand-file-name "test-work" default-directory)))
    (unless (file-directory-p test-directory)
      (make-directory test-directory t))
    (let ((default-directory (file-name-as-directory test-directory)))
      (let ((file (concat (make-temp-name "test-data") ".txt")))
        (pcase data
          ((pred stringp)
           (wgrep-test-helper--prepare-file file data))
          (`(,(and (pred stringp) data) ,(and (pred coding-system-p) cs))
           (wgrep-test-helper--prepare-file file data cs))
          (_
           (error "DATA should be STRING or (STRING CODING-SYSTEM)")))
        (unwind-protect
            (funcall body-fn file)
          (wgrep-test-helper--cleanup-file file))))))

(put 'wgrep-test-helper-fixture 'lisp-indent-function 1)

(provide 'wgrep-test-helper)
