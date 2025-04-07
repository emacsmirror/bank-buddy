;; bank-buddy-test.el --- Tests for bank-buddy.el package

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your-email@example.com>
;; Keywords: testing, finance
;; Package-Requires: ((emacs "26.1") (bank-buddy "1.1") (async "1.9.4") (ert "0.3"))

;;; Commentary:
;;
;; This file provides testing for the bank-buddy financial analysis package.
;; It includes tests to verify that the CSV parsing, categorization, and report
;; generation are functioning correctly.

;;; Code:

(require 'bank-buddy)
(require 'ert)
;;(require 'ert-async)
(require 'cl-lib)

(defvar bank-buddy-test-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the bank-buddy-test.el file.")

(defvar bank-buddy-test-csv (expand-file-name "test-bank-statement.csv" bank-buddy-test-dir)
  "Path to the test CSV file.")

(defvar bank-buddy-test-expected-report (expand-file-name "expected-report.org" bank-buddy-test-dir)
  "Path to the expected report file.")

(defvar bank-buddy-test-output-report (expand-file-name "test-output-report.org" bank-buddy-test-dir)
  "Path to the test output report file.")

(defun bank-buddy-test--normalize-report (report-file)
  "Normalize the content of REPORT-FILE for comparison.
Removes date/time stamps and other variable content."
  (with-temp-buffer
    (insert-file-contents report-file)
    ;; Remove the date line which will always be different
    (goto-char (point-min))
    (when (re-search-forward "^#\\+date:.*$" nil t)
      (replace-match "#DATE-REMOVED-FOR-COMPARISON"))
    ;; Normalize any other variable parts that might change between runs
    ;; For example, exact calculations might vary slightly due to rounding differences
    ;; You might want to add more normalization steps here
    (buffer-string)))

(defun bank-buddy-test--reports-equal-p (file1 file2)
  "Compare two report files FILE1 and FILE2, ignoring variable content."
  (let ((content1 (bank-buddy-test--normalize-report file1))
        (content2 (bank-buddy-test--normalize-report file2)))
    (string= content1 content2)))

;; Utility to create our test file
(defun bank-buddy-test--setup-test-files ()
  "Set up the test files needed for testing."
  (unless (file-exists-p bank-buddy-test-dir)
    (make-directory bank-buddy-test-dir t))
  
  ;; Create test CSV file if it doesn't exist
  (unless (file-exists-p bank-buddy-test-csv)
    (with-temp-file bank-buddy-test-csv
      (insert "Date,Description,Amount
2024-01-05,PAYPAL TRANSFER,55.99
2024-01-07,SKY-BETTING RACES,20.00
2024-01-10,VIRGIN-MEDIA MONTHLY,65.50
2024-01-12,PAYPAL TRANSFER,22.45
2024-01-15,NETFLIX SUBSCRIPTION,13.99
2024-01-18,SAINSBURYS GROCERIES,88.75
2024-01-20,AMAZON PURCHASE,45.30
2024-01-25,THREE MOBILE,25.00
2024-01-28,STARBUCKS COFFEE,4.95
2024-01-30,UBER RIDE,12.50
2024-02-01,KATHERINE ALLOWANCE,200.00
2024-02-03,RAILWAY TICKET,35.45
2024-02-05,ROYAL-MAIL POSTAGE,8.95
2024-02-07,NETFLIX SUBSCRIPTION,13.99
2024-02-10,AMAZON PURCHASE,65.75
2024-02-12,WATERSTONES BOOK,15.99
2024-02-15,TESCO GROCERIES,75.40
2024-02-18,PAYPAL TRANSFER,35.99
2024-02-20,DISNEY+ SUBSCRIPTION,7.99
2024-02-23,ASDA GROCERIES,92.45
2024-02-25,SKY SUBSCRIPTION,45.99
2024-02-28,YOUTUBE PREMIUM,11.99
2024-03-01,VIRGIN-MEDIA MONTHLY,65.50
2024-03-03,NEXT RETAIL-LTD,125.00
2024-03-05,NATWEST-BANK-REFERENCE RENT,750.00
2024-03-07,NETFLIX SUBSCRIPTION,13.99
2024-03-10,AMAZON PURCHASE,28.99
2024-03-12,RAILWAY TICKET,22.50
2024-03-15,SPECSAVERS APPOINTMENT,25.00
2024-03-17,SAINSBURYS GROCERIES,95.25
2024-03-20,THREE MOBILE,25.00
2024-03-22,WITHDRAWAL ATM,50.00
2024-03-25,UBER RIDE,15.75
2024-03-28,BET365 RACES,30.00
2024-03-30,PAYPAL TRANSFER,45.00
2024-04-01,KATHERINE ALLOWANCE,200.00
2024-04-03,VIRGIN-MEDIA MONTHLY,65.50
2024-04-05,CLAUDE SUBSCRIPTION,20.00
2024-04-07,NETFLIX SUBSCRIPTION,13.99
2024-04-10,AMAZON PURCHASE,78.50
2024-04-12,STARBUCKS COFFEE,9.85
2024-04-15,TESCO GROCERIES,68.95
2024-04-18,IKEA FURNITURE,245.99
2024-04-20,YOUTUBE PREMIUM,11.99
2024-04-23,RAILWAY TICKET,18.50
2024-04-25,PETS AT HOME,45.00
2024-04-28,PRIME VIDEO RENTAL,4.99
2024-04-30,DELIVEROO FOOD,25.50
2024-05-02,KATHERINE ALLOWANCE,200.00
2024-05-05,VIRGIN-MEDIA MONTHLY,65.50
2024-05-07,NETFLIX SUBSCRIPTION,13.99
2024-05-10,AMAZON PURCHASE,32.99
2024-05-12,SAINSBURYS GROCERIES,105.75
2024-05-15,UBER RIDE,18.25
2024-05-18,THREE MOBILE,25.00
2024-05-20,PAYPAL TRANSFER,40.00
2024-05-22,NOWTV SUBSCRIPTION,9.99
2024-05-25,WAITROSE GROCERIES,115.45
2024-05-28,SKY-BETTING RACES,25.00
2024-05-30,DENTIST APPOINTMENT,60.00
2024-06-01,KATHERINE ALLOWANCE,200.00
2024-06-03,NOTEMACHINE WITHDRAWAL,100.00
2024-06-05,VIRGIN-MEDIA MONTHLY,65.50
2024-06-07,NETFLIX SUBSCRIPTION,13.99
2024-06-10,AMAZON PURCHASE,55.25
2024-06-12,RAILWAY TICKET,42.00
2024-06-15,SPOTIFY PREMIUM,9.99
2024-06-18,RIVER-ISLAND CLOTHES,85.99
2024-06-20,THREE MOBILE,25.00
2024-06-22,AUDIBLE SUBSCRIPTION,7.99
2024-06-25,ASDA GROCERIES,78.50
2024-06-28,JUST-EAT TAKEAWAY,32.99
2024-06-30,PAYPAL TRANSFER,28.50")))
  
  ;; Create expected report file if it doesn't exist
  (unless (file-exists-p bank-buddy-test-expected-report)
    (with-temp-file bank-buddy-test-expected-report
      ;; Insert the expected report content
      (insert-file-contents (expand-file-name "expected-report-template.org" bank-buddy-test-dir)))))

(defun bank-buddy-test--cleanup ()
  "Clean up temporary test files."
  (when (file-exists-p bank-buddy-test-output-report)
    (delete-file bank-buddy-test-output-report)))

;;;###autoload
(ert-deftest bank-buddy-test-report-generation ()
  "Test bank-buddy report generation against expected output."
  (bank-buddy-test--setup-test-files)
  (unwind-protect
      (progn
        ;; Generate the report using bank-buddy
        (bank-buddy-generate-report bank-buddy-test-csv bank-buddy-test-output-report)
        
        ;; Wait for async processing to complete and the file to be written
        (let ((timeout 30)  ; 30 seconds timeout
              (interval 0.5)) ; Check every 0.5 seconds
          (while (and (> timeout 0)
                      (not (file-exists-p bank-buddy-test-output-report)))
            (sleep-for interval)
            (setq timeout (- timeout interval))))
        
        ;; Verify the file was created
        (should (file-exists-p bank-buddy-test-output-report))
        
        ;; Compare the generated report with the expected report
        (should (bank-buddy-test--reports-equal-p 
                 bank-buddy-test-output-report
                 bank-buddy-test-expected-report)))
    ;; Clean up
    (bank-buddy-test--cleanup)))

;;;###autoload
(defun bank-buddy-run-tests ()
  "Run all bank-buddy tests."
  (interactive)
  (ert "^bank-buddy-test"))

;; Create a custom test file generator function for easy setup
;;;###autoload
(defun bank-buddy-test-create-test-files ()
  "Create test files needed for bank-buddy testing."
  (interactive)
  (bank-buddy-test--setup-test-files)
  (message "Bank Buddy test files created in %s" bank-buddy-test-dir))

(provide 'bank-buddy-test)

;;; bank-buddy-test.el ends here
