(require 'ert)
(require 'wgrep-test-helper)
(require 'ag)

(ert-deftest wgrep-ag-normal ()
  :tags '(wgrep-subtest)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture "HOGE\nFOO\nBAZ\n"
     (lambda (file)
       (wgrep-test-helper--ag "FOO|HOGE" file)
       (wgrep-change-to-wgrep-mode)
       (goto-char (point-min))
       (wgrep-goto-first-found)
       ;; search hit line (hit by -C option)
       (should (re-search-forward "HOGE" nil t))
       ;; delete 1st line
       (wgrep-mark-deletion)
       (should (re-search-forward "FOO" nil t))
       ;; replace 2nd line
       (replace-match "FOO2")
       ;; apply to buffer
       (wgrep-finish-edit)
       ;; save to file
       (wgrep-save-all-buffers)
       ;; compare file contents is valid
       (should (equal "FOO2\nBAZ\n" (wgrep-test-helper--get-contents file)))))))

(ert-deftest wgrep-deadgrep-normal ()
  :tags '(wgrep-subtest)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture "HOGE\nFOO\nBAZ\n"
     (lambda (file)
       (wgrep-test-helper--deadgrep "FOO|HOGE")
       (wgrep-change-to-wgrep-mode)
       (goto-char (point-min))
       (wgrep-goto-first-found)
       ;; search hit line (hit by -C option)
       (should (re-search-forward "HOGE" nil t))
       ;; delete 1st line
       (wgrep-mark-deletion)
       (should (re-search-forward "FOO" nil t))
       ;; replace 2nd line
       (replace-match "FOO2")
       ;; apply to buffer
       (wgrep-finish-edit)
       ;; save to file
       (wgrep-save-all-buffers)
       ;; compare file contents is valid
       (should (equal "FOO2\nBAZ\n" (wgrep-test-helper--get-contents file)))))))
