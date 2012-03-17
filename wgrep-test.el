(require 'ert)

(defun wgrep-test--grep (command)
  (let* ((buf (grep command))
         (proc (get-buffer-process buf)))
    (while (eq (process-status proc) 'run) 
      (sit-for 0.1))
    (switch-to-buffer buf)))

(defun wgrep-test--contents (file &optional cs)
  (let ((coding-system-for-read cs))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun wgrep-test--file (file contents)
  ;; cleanup for convinience
  (let ((buf (get-file-buffer file)))
    (kill-buffer buf))
  (write-region contents nil file))

(ert-deftest wgrep-normal ()
  :tags '(wgrep)
  (wgrep-test--file "test-data.txt" "HOGE\nFOO\nBAZ\n")
  (wgrep-test--grep "grep -nH -e FOO -C 1 test-data.txt")
  (wgrep-change-to-wgrep-mode)
  (goto-char (point-min))
  ;; header is readonly
  (should (re-search-forward "^grep" nil t))
  (should-error (delete-char 1) :type 'text-read-only)
  ;; search hit line (hit by -C option)
  (should (re-search-forward "HOGE" nil t))
  ;; delete 1st line
  (wgrep-flush-current-line)
  (should (re-search-forward "FOO" nil t))
  ;; replace 2nd line
  (replace-match "FOO2")
  ;; footer is readonly
  (goto-char (point-max))
  (should-error (delete-char -1) :type 'text-read-only)
  ;; apply to buffer
  (wgrep-finish-edit)
  ;; save to file
  (wgrep-save-all-buffers)
  ;; compare file contents is valid
  (should (equal "FOO2\nBAZ\n" (wgrep-test--contents "test-data.txt")))
  (delete-file "test-data.txt"))


;; TODO 
;; * utf-8 bom
;; * multibyte character
;; * wgrep-toggle-readonly-area
;; * wgrep-abort-changes
;; * wgrep-exit
