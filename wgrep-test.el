(require 'ert)
(require 'dash)
(require 's)
(require 'wgrep-test-helper)

(ert-deftest wgrep-normal ()
  :tags '(wgrep)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture "HOGE\nFOO\nBAZ\n"
     (lambda (file)
       (wgrep-test-helper--grep (concat "grep -nH -e FOO -C 1 " file))
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
       (should (equal "FOO2\nBAZ\n" (wgrep-test-helper--get-contents file)))))))

(ert-deftest wgrep-normal-with-newline ()
  :tags '(wgrep)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture "HOGE\n"
     (lambda (file)
       (wgrep-test-helper--grep (concat "grep -nH -e HOGE " file))
       (wgrep-change-to-wgrep-mode)
       (goto-char (point-min))
       ;; through the header
       (should (re-search-forward (concat "^" (regexp-quote file) ":") nil t))
       ;; search hit line (hit by -C option)
       (should (re-search-forward "HOGE" nil t))
       (replace-match "FOO\nBAZ")
       ;; apply to buffer
       (wgrep-finish-edit)
       ;; save to file
       (wgrep-save-all-buffers)
       ;; compare file contents is valid
       (should (equal "FOO\nBAZ\n" (wgrep-test-helper--get-contents file)))))))

(ert-deftest wgrep-bom-with-multibyte ()
  :tags '(wgrep)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture '("あ\nい\nう\n" utf-8-with-signature)
     (lambda (file)
       (wgrep-test-helper--grep (concat "grep -nH -e 'あ' -A 2 " file))
       (wgrep-change-to-wgrep-mode)
       (goto-char (point-min))
       ;; BOM check is valid.
       ;; skip BOM by `.*'
       (should (re-search-forward (concat (regexp-quote file) ":[0-9]+:.*\\(あ\\)$") nil t))
       (replace-match "へのへのも" nil nil nil 1)
       ;; 2nd line
       (should (re-search-forward (concat (regexp-quote file) "-[0-9]+-\\(い\\)$") nil t))
       (replace-match "へじ" nil nil nil 1)
       ;; apply to buffer
       (wgrep-finish-edit)
       ;; save to file
       (wgrep-save-all-buffers)
       ;; compare file contents is valid
       (should (equal "へのへのも\nへじ\nう\n" (wgrep-test-helper--get-contents file)))
       ))))

(ert-deftest wgrep-bom-with-unibyte ()
  :tags '(wgrep)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture '("a\nb\n" utf-8-with-signature)
     (lambda (file)
       (wgrep-test-helper--grep (concat "grep -nH -e 'a' -A 2 " file))
       (wgrep-change-to-wgrep-mode)
       (goto-char (point-min))
       ;; BOM check is valid.
       (should (re-search-forward (concat (regexp-quote file) ":[0-9]+:.*\\(a\\)$") nil t))
       (replace-match "ABCD" nil nil nil 1)
       ;; apply to buffer
       (wgrep-finish-edit)
       ;; save to file
       (wgrep-save-all-buffers)
       ;; compare file contents is valid
       (should (equal "ABCD\nb\n" (wgrep-test-helper--get-contents file)))))))

(ert-deftest wgrep-with-modify ()
  :tags '(wgrep)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture "a\nb\nc\n"
     (lambda (file)
       (let (;; This test intended to check modified buffer is existing.
             ;; Keep that buffer is modifying while calling grep.
             (grep-save-buffers nil))
         (with-current-buffer (find-file-noselect file)
           ;; modify file buffer
           (goto-char (point-min))
           (and (re-search-forward "^a" nil t)
                (replace-match "hoge"))
           (and (re-search-forward "^b" nil t)
                (replace-match "foo")))
         (wgrep-test-helper--grep (concat "grep -nH -e 'a' -A 2 " file))
         (wgrep-change-to-wgrep-mode)
         (goto-char (point-min))
         ;; delete "a" line (failed when saving)
         (should (re-search-forward (concat (regexp-quote file) ":[0-9]+:.*\\(a\\)$") nil t))
         (wgrep-mark-deletion)
         ;; replace "b" line (failed when saving)
         (should (re-search-forward (concat (regexp-quote file) "-[0-9]+-.*\\(b\\)$") nil t))
         (replace-match "B" nil nil nil 1)
         ;; replace "c" line
         (should (re-search-forward (concat (regexp-quote file) "-[0-9]+-.*\\(c\\)$") nil t))
         (replace-match "C" nil nil nil 1)
         ;; apply to buffer
         (wgrep-finish-edit)
         ;; save to file
         (wgrep-save-all-buffers)
         ;; compare file contents is valid. (keep preceding file buffer's contents)
         (should (equal "hoge\nfoo\nC\n" (wgrep-test-helper--get-contents file))))))))

(ert-deftest wgrep-with-readonly-file ()
  :tags '(wgrep)
  (wgrep-test-helper--default
   (wgrep-test-helper-fixture "a\nb\nc\n"
     (lambda (file)
       ;; make readonly
       (set-file-modes file ?\400)
       (wgrep-test-helper--grep (concat "grep -nH -e 'a' " file))
       (wgrep-change-to-wgrep-mode)
       (goto-char (point-min))
       (should (re-search-forward (concat (regexp-quote file) ":[0-9]+:.*\\(a\\)$") nil t))
       (replace-match "A" nil nil nil 1)
       ;; only check with no error
       (wgrep-finish-edit)

       ;; TODO check result file is unchanged
       ))))

;; TODO (Not implemented testcase)
;; * wgrep-toggle-readonly-area
;; ** sort-lines
;; * wgrep-abort-changes
;; * wgrep-exit
;; * broken file contents (invalid coding system)
;; * new text contains newline
;; * wgrep-change-readonly-file
;; * test wgrep-*.el

(provide 'wgrep-test)
