;; Example usage of the bank-buddy package

;; 1. First, load the package
(use-package bank-buddy
  :load-path "~/source/repos/bank-buddy")

;; 2. Generate a financial report from a CSV file
(bank-buddy-generate-report "bank-statement.csv" "financial-report.org")

;; 3. Customization options
;; Exclude large transactions (over £10,000)
(setq bank-buddy-exclude-large-txns t)
(setq bank-buddy-large-txn-threshold 10000)

;; Adjust subscription detection sensitivity
(setq bank-buddy-subscription-min-occurrences 3)

;; 4. Manual processing (if you want more control)
;; Parse the CSV file
(bank-buddy-parse-csv-file "bank-statement.csv")

;; Process all the payments
(bank-buddy-process-payments)

;; Generate a custom report
(with-temp-buffer
  (insert "#+title: Custom Financial Report\n\n")
  
  ;; Include only the sections you want
  (bank-buddy-generate-summary-overview)
  (bank-buddy-generate-top-spending-categories)
  (bank-buddy-generate-monthly-spending)
  
  ;; Save to file
  (write-file "custom-report.org"))

;; 5. Debug information (for troubleshooting)
(bank-buddy-debug-info)
