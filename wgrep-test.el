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

(defun wgrep-test--prepare (file contents &optional cs)
  ;; cleanup for convinience
  (let ((buf (get-file-buffer file)))
    (kill-buffer buf))
  (let ((coding-system-for-write cs))
    (write-region contents nil file)))

(ert-deftest wgrep-normal ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare "test-data.txt" "HOGE\nFOO\nBAZ\n")
    (wgrep-test--grep "grep -nH -e FOO -C 1 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; header is readonly
    (should (re-search-forward "^grep" nil t))
    (should-error (delete-char 1) :type 'text-read-only)
    ;; search hit line (hit by -C option)
    (should (re-search-forward "HOGE" nil t))
    ;; delete 1st line
    (wgrep-mark-deletion)
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
    (delete-file "test-data.txt")))

(ert-deftest wgrep-normal-with-newline ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare "test-data.txt" "HOGE\n")
    (wgrep-test--grep "grep -nH -e HOGE test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; through the header
    (should (re-search-forward "^test-data\\.txt:" nil t))
    ;; search hit line (hit by -C option)
    (should (re-search-forward "HOGE" nil t))
    (replace-match "FOO\nBAZ")
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "FOO\nBAZ\n" (wgrep-test--contents "test-data.txt")))
    (delete-file "test-data.txt")))

(ert-deftest wgrep-bom-with-multibyte ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare "test-data.txt" "あ\nい\nう\n" 'utf-8-with-signature)
    (wgrep-test--grep "grep -nH -e 'あ' -A 2 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; BOM check is valid.
    ;; skip BOM by `.*'
    (should (re-search-forward "test-data\\.txt:[0-9]+:.*\\(あ\\)$" nil t))
    (replace-match "へのへのも" nil nil nil 1)
    ;; 2nd line
    (should (re-search-forward "test-data\\.txt-[0-9]+-\\(い\\)$" nil t))
    (replace-match "へじ" nil nil nil 1)
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "へのへのも\nへじ\nう\n" (wgrep-test--contents "test-data.txt")))
    (delete-file "test-data.txt")))

(ert-deftest wgrep-bom-with-unibyte ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare "test-data.txt" "a\nb\n" 'utf-8-with-signature)
    (wgrep-test--grep "grep -nH -e 'a' -A 2 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; BOM check is valid.
    (should (re-search-forward "test-data\\.txt:[0-9]+:.*\\(a\\)$" nil t))
    (replace-match "ABCD" nil nil nil 1)
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "ABCD\nb\n" (wgrep-test--contents "test-data.txt")))
    (delete-file "test-data.txt")))

(ert-deftest wgrep-with-modify ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare "test-data.txt" "a\nb\nc\n")
    (with-current-buffer (find-file-noselect "test-data.txt")
      ;; modify file buffer
      (goto-char (point-min))
      (and (re-search-forward "^a" nil t)
           (replace-match "hoge"))
      (and (re-search-forward "^b" nil t)
           (replace-match "foo")))
    (wgrep-test--grep "grep -nH -e 'a' -A 2 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; delete "a" line (failed when saving)
    (should (re-search-forward "test-data\\.txt:[0-9]+:.*\\(a\\)$" nil t))
    (wgrep-mark-deletion)
    ;; replace "b" line (failed when saving)
    (should (re-search-forward "test-data\\.txt-[0-9]+-.*\\(b\\)$" nil t))
    (replace-match "B" nil nil nil 1)
    ;; replace "c" line
    (should (re-search-forward "test-data\\.txt-[0-9]+-.*\\(c\\)$" nil t))
    (replace-match "C" nil nil nil 1)
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "hoge\nfoo\nC\n" (wgrep-test--contents "test-data.txt")))
    (delete-file "test-data.txt")))

;; TODO 
;; * wgrep-toggle-readonly-area
;; ** sort-lines
;; * wgrep-abort-changes
;; * wgrep-exit
;; * broken file contents (invalid coding system)
;; * new text contains newline
;; * wgrep-change-readonly-file
