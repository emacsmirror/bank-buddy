;;; bank-buddy.el --- Enhanced financial analysis and reporting for Emacs (Async Version)

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your-email@example.com>
;; Version: 1.1 (Async)
;; Package-Requires: ((emacs "26.1") (csv "0.5") (async "1.9.4")) ;; Added async
;; Keywords: finance, budget, reporting
;; URL: https://github.com/yourusername/bank-buddy

;;; Commentary:
;;
;; This package provides enhanced financial analysis and reporting capabilities
;; for bank statement data. It builds on existing categorization functionality
;; to provide comprehensive financial reports including:
;;
;; - Transaction summaries and overviews
;; - Spending category analysis
;; - Top merchant identification
;; - Monthly spending patterns
;; - Recurring subscription detection
;;
;; The package reads CSV bank statement data *asynchronously*, categorizes
;; transactions, and generates detailed reports in Org-mode format.
;; The CSV parsing and processing happens in a background process to avoid
;; blocking Emacs.

;;; Code:

(require 'csv)
(require 'cl-lib)
(require 'async) ;; Require the async library

(defvar bank-buddy-unmatched-transactions '()
  "List of transactions that matched only the catch-all pattern.")

;; Variables and data structures (remain global for report generation)
(defvar bank-buddy-payments '()
  "List of parsed payment transactions. Populated by async callback.")

(defvar bank-buddy-cat-tot (make-hash-table :test 'equal)
  "Hash table storing category totals. Populated by async callback.")

(defvar bank-buddy-merchants (make-hash-table :test 'equal)
  "Hash table storing merchant totals. Populated by async callback.")

(defvar bank-buddy-monthly-totals (make-hash-table :test 'equal)
  "Hash table storing monthly spending totals. Populated by async callback.")

(defvar bank-buddy-txn-size-dist (make-hash-table :test 'equal)
  "Hash table for tracking transaction size distribution. Populated by async callback.")

(defvar bank-buddy-subs (make-hash-table :test 'equal)
  "Hash table for tracking potential subscriptions. Populated by async callback.")

(defvar bank-buddy-date-first nil
  "First transaction date. Populated by async callback.")

(defvar bank-buddy-date-last nil
  "Last transaction date. Populated by async callback.")

;; Customization options (remain the same)
(defgroup bank-buddy nil
  "Customization options for bank-buddy."
  :group 'applications)

(defcustom bank-buddy-exclude-large-txns t
  "Whether to exclude transactions over £10,000."
  :type 'boolean
  :group 'bank-buddy)

(defcustom bank-buddy-large-txn-threshold 10000
  "Threshold for large transactions in pounds."
  :type 'number
  :group 'bank-buddy)

(defcustom bank-buddy-subscription-min-occurrences 3
  "Minimum occurrences for subscription detection."
  :type 'number
  :group 'bank-buddy)

;; Category mappings and names (remain the same)
(defvar bank-buddy-cat-list-defines
  '(("katherine\\|lucinda\\|kate" "kat")
    ("railw\\|railway\\|selfserve\\|train" "trn")
    ("paypal" "pay")
    ("virgin-media\\|uinsure\\|insurance\\|royal-mail\\|postoffice\\|endsleigh\\|waste\\|lloyds\\|electric\\|sse\\|newsstand\\|privilege\\|pcc\\|licence\\|ovo\\|energy\\|bt\\|water" "utl")
    ("sky-betting\\|b365\\|races\\|bet365\\|racing" "bet")
    ("stakeholde\\|widows" "pen")
    ("nsibill\\|vines\\|ns&i\\|saver" "sav")
    ("uber\\|aqua" "txi")
    ("magazine\\|specs\\|zinio\\|specsavers\\|publishing\\|anthem\\|kindle\\|news" "rdg")
    ("claude\\|escape\\|deviant\\|cleverbridge\\|reddit\\|pixel\\|boox\\|ionos\\|microsoft\\|mobile\\|backmarket\\|cartridge\\|whsmith\\|dazn\\|my-picture\\|openai\\|c-date\\|ptitis\\|keypmt\\|billnt\\|fee2nor\\|assistance\\|boxise\\|billkt\\|paintstor\\|iet-main\\|ffnhelp\\|shadesgrey\\|venntro\\|vtsup\\|sunpts\\|apyse\\|palchrge\\|maypmt\\|filemodedesk\\|istebrak\\|connective\\|avangate\\|stardock\\|avg\\|123\\|web\\|a2" "web")
    ("notemachine\\|anchrg\\|hilsea\\|withdrawal" "atm")
    ("finance" "fin")
    ("youtube\\|entertai\\|twitch\\|disney\\|box-office\\|discovery\\|tvplayer\\|vue\\|sky\\|netflix\\|audible\\|nowtv\\|channel\\|prime" "str")
    ("platinum\\|card" "crd")
    ("top-up\\|three\\|h3g" "phn")
    ("amaz\\|amz" "amz")
    ("pets\\|pet" "pet")
    ("mydentist\\|dentist" "dnt")
    ("natwest-bank-reference\\|residential\\|rent\\|yeong" "hse")
    ("mardin\\|starbuck\\|gillett-copnor\\|asda\\|morrison\\|sainsburys\\|waitrose\\|tesco\\|domino\\|deliveroo\\|just.*eat" "fod")
    ("retail-ltd\\|vinted\\|lockart\\|moment-house\\|yuyu\\|bushra\\|newhome\\|white-barn\\|skinnydip\\|mgs\\|river-island\\|spencer\\|lilian\\|jung\\|ikea\\|wayfair\\|neom\\|teespring\\|lick-home\\|matalan\\|devon-wick\\|united-arts\\|lush-retail\\|lisa-angel\\|sharkninja\\|fastspring\\|bonas\\|asos\\|emma\\|sofology\\|ebay\\|dunelm\\|coconut\\|semantical\\|truffle\\|nextltd\\|highland\\|little-crafts\\|papier\\|the-hut\\|new-look\\|samsung\\|astrid\\|pandora\\|waterstone\\|cultbeauty\\|24pymt\\|champo\\|costa\\|gollo\\|pumpkin\\|argos\\|the-range\\|biffa\\|moonpig\\|apple\\|itunes\\|gold\\|interflora\\|thortful" "shp")
    ("js-law" "law")
    ("anyvan" "hmv")
    (".*" "o"))
  "Categorization patterns for transactions.")

(defvar bank-buddy-category-names
  '(("kat" . "Personal (Katherine)")
    ("trn" . "Transport")
    ("pay" . "PayPal")
    ("utl" . "Utilities")
    ("bet" . "Betting")
    ("pen" . "Pension")
    ("sav" . "Savings")
    ("txi" . "Taxi")
    ("rdg" . "Reading")
    ("web" . "Web Services")
    ("atm" . "Cash Withdrawals")
    ("fin" . "Finance")
    ("str" . "Streaming")
    ("crd" . "Credit Card")
    ("phn" . "Phone")
    ("amz" . "Amazon")
    ("pet" . "Pet Expenses")
    ("dnt" . "Dental")
    ("hse" . "Housing")
    ("fod" . "Food")
    ("shp" . "Shopping")
    ("law" . "Legal")
    ("hmv" . "Moving")
    ("o" . "Other"))
  "Human-readable category names for reporting.")

(defvar bank-buddy-subscription-patterns
  '(("RACINGTV" . "Racing TV")
    ("GOOGLE" . "Google Play")
    ("PRIME VIDEO" . "Prime Video")
    ("YOUTUBE" . "YouTube Premium")
    ("NOW TV" . "NOW TV")
    ("DELIVEROO PLUS" . "Deliveroo Plus")
    ("AMAZON PRIME" . "Amazon Prime")
    ("SAINSBURY.*PASS" . "Sainsbury's Delivery Pass")
    ("CLAUDE" . "Claude.ai")
    ("NETFLIX" . "Netflix")
    ("DISNEY" . "Disney+")
    ("SPOTIFY" . "Spotify")
    ("APPLE.*ONE" . "Apple One"))
  "Patterns to identify specific subscriptions.")

(defun bank-buddy-generate-unmatched-transactions ()
  "Generate a section showing transactions that weren't matched by specific patterns."
  (insert "\n* Unmatched Transactions\n\n")
  (insert "The following transactions were only matched by the catch-all pattern (\".*\"). ")
  (insert "You may want to add specific patterns for these in `bank-buddy-cat-list-defines`.\n\n")
  
  (if bank-buddy-unmatched-transactions
      (progn
        (insert "#+begin_src text\n")
        (dolist (txn (sort bank-buddy-unmatched-transactions #'string<))
          (insert (format "%s\n" txn)))
        (insert "#+end_src\n"))
    (insert "All transactions were matched by specific patterns.\n")))

(defun bank-buddy-show-progress (message &optional append)
  "Show progress MESSAGE in a dedicated buffer.
If APPEND is non-nil, append to existing content."
  (let ((buf (get-buffer-create "*Bank Buddy Progress*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (if (not append)
            (erase-buffer)
          (goto-char (point-max))
          (unless (bolp) (insert "\n")))
        (insert (format "[%s] %s" (format-time-string "%H:%M:%S") message))
        (unless (looking-at "$") (insert "\n"))))
    ;; Display buffer if not already visible
    (unless (get-buffer-window buf)
      (display-buffer buf))))

;; Helper function for date calculation (no change needed)
(defun bank-buddy-days-between (date1 date2)
  "Calculate days between DATE1 and DATE2 in YYYY-MM-DD format."
  (let ((time1 (date-to-time date1))
        (time2 (date-to-time date2)))
    (floor (/ (float-time (time-subtract time2 time1)) 86400))))

(defun bank-buddy--categorize-payment-local (name debit month date cat-tot merchants monthly-totals txn-size-dist subs)
  "Categorize payment based on NAME, DEBIT amount, MONTH and DATE, updating local hash tables."
  (let ((category-found nil)
        (split-key nil)
        (merchant (replace-regexp-in-string " .*" "" name))
        (unmatched t)) ; New flag to track if transaction matched only catch-all

    ;; Find category (uses global bank-buddy-cat-list-defines read by the child process)
    (cl-loop for category in bank-buddy-cat-list-defines
             when (string-match-p (nth 0 category) name)
             do (progn
                  (setq category-found (nth 1 category))
                  ;; If it's not the catch-all pattern ".*", mark as matched
                  (when (not (string= (nth 0 category) ".*"))
                    (setq unmatched nil)))
             and return t)
    (unless category-found (setq category-found "o")) ; Ensure a category

    ;; If transaction is unmatched (only matched by ".*"), add to local list
    (when unmatched
      (push name bank-buddy-unmatched-transactions-local))

    ;; Rest of the function remains unchanged
    ;; Update category totals (local hash)
    (setq split-key (concat month "-" category-found))
    (puthash split-key
             (+ (gethash split-key cat-tot 0) debit)
             cat-tot)

    ;; Update monthly totals (local hash)
    (puthash month
             (+ (gethash month monthly-totals 0) debit)
             monthly-totals)

    ;; Update merchant totals (local hash)
    (puthash merchant
             (+ (gethash merchant merchants 0) debit)
             merchants)

    ;; Update transaction size distribution (local hash)
    (cond
     ((<= debit 10)
      (puthash "under-10" (1+ (gethash "under-10" txn-size-dist 0)) txn-size-dist))
     ((<= debit 50)
      (puthash "10-to-50" (1+ (gethash "10-to-50" txn-size-dist 0)) txn-size-dist))
     ((<= debit 100)
      (puthash "50-to-100" (1+ (gethash "50-to-100" txn-size-dist 0)) txn-size-dist))
     (t
      (puthash "over-100" (1+ (gethash "over-100" txn-size-dist 0)) txn-size-dist)))

    ;; Track potential subscriptions (local hash, uses global bank-buddy-subscription-patterns)
    (cl-loop for (pattern . sub-name) in bank-buddy-subscription-patterns
             when (string-match-p pattern (upcase name))
             do (let* ((sub-key (concat sub-name "-" (format "%.2f" debit)))
                       (dates (gethash sub-key subs nil)))
                  (puthash sub-key
                           (cons (cons date debit) dates)
                           subs)))

    ;; Return the assigned category (though not strictly needed by caller in async context)
    category-found))

;; The core worker function to be run asynchronously.
(defun bank-buddy--process-csv-async-worker (csv-file)
  "Parse CSV-FILE and process payments. Returns processed data as a list.
This function runs in a separate process via async.el."
  ;; Ensure required libraries are loaded in the child process
  (require 'csv)
  (require 'cl-lib)
  ;; It also implicitly needs bank-buddy loaded for helper functions and variables.
  ;; Assuming bank-buddy is 'required' or 'loaded' via the async-start call.

  ;; --- Use local variables, DO NOT modify globals directly ---
  (let ((payments '())
        (cat-tot (make-hash-table :test 'equal))
        (merchants (make-hash-table :test 'equal))
        (monthly-totals (make-hash-table :test 'equal))
        (txn-size-dist (make-hash-table :test 'equal))
        (subs (make-hash-table :test 'equal))
        (bank-buddy-unmatched-transactions-local '()) ; New local list for unmatched transactions
        (date-first nil)
        (date-last nil)
        (error-occurred nil))

    ;; --- 1. Parsing (similar to bank-buddy-parse-csv-file) ---
    (condition-case err
        (with-temp-buffer
          (insert-file-contents csv-file)
          ;; Use csv-parse-region for better error handling potential
          (setq payments (csv-parse-buffer t)))
      (error (setq error-occurred (format "Error parsing CSV: %S" err))))

    ;; --- 2. Processing (similar to bank-buddy-process-payments) ---
    (unless error-occurred
      (condition-case err
          (progn
            ;; --- 2a. Calculate Date Range ---
            (when payments
              (let ((dates (mapcar (lambda (payment)
                                     (cdr (nth 0 payment)))
                                   payments)))
                (when dates ; Ensure dates list is not empty
                  (condition-case date-err ; Handle potential errors in date parsing/sorting
                      (progn
                        (setq date-first (car (sort (copy-sequence dates) #'string<)))
                        (setq date-last (car (sort (copy-sequence dates) #'string>))))
                    (error (setq error-occurred (format "Error calculating date range: %S" date-err)))))))

            ;; --- 2b. Process Payments loop ---
            (when (and payments (not error-occurred))
              (let ((total-transactions 0)
                    (total-amount 0))
                (dolist (payment payments)
                  (let* ((date-cell (nth 0 payment))
                         (desc-cell (nth 1 payment))
                         (debit-cell (nth 2 payment))
                         (date (if date-cell (cdr date-cell) "")) ; Handle potentially nil cells
                         (month (if (>= (length date) 7) (substring date 0 7) "UNKNOWN"))
                         (description (if desc-cell (cdr desc-cell) ""))
                         (debit-str (if debit-cell (cdr debit-cell) ""))
                         (debit (if (string-blank-p debit-str) 0 (string-to-number debit-str))))

                    ;; Skip large transactions if configured
                    (when (and (numberp debit) ; Ensure debit is a number
                               (or (not bank-buddy-exclude-large-txns)
                                   (< debit bank-buddy-large-txn-threshold)))
                      (when (> debit 0) ;; Only count positive debits
                        ;; Call the local categorizer, passing local hash tables
                        (bank-buddy--categorize-payment-local
                         (replace-regexp-in-string " " "-" description)
                         debit month date
                         cat-tot merchants monthly-totals txn-size-dist subs)
                        (setq total-transactions (1+ total-transactions))
                        (setq total-amount (+ total-amount debit)))))))))
        (error (setq error-occurred (format "Error processing payments: %S" err)))))


    ;; Return all the results needed for report generation as a list.
    ;; Include error status and unmatched transactions.
    (list :error error-occurred ; nil if no error
          :date-first date-first
          :date-last date-last
          :cat-tot cat-tot
          :merchants merchants
          :monthly-totals monthly-totals
          :txn-size-dist txn-size-dist
          :subs subs
          :unmatched-transactions bank-buddy-unmatched-transactions-local
          )))

;; --- Reporting Functions (remain largely the same, operate on global vars) ---
;; These functions assume the global variables have been populated by the async callback.

(defun bank-buddy-generate-summary-overview ()
  "Generate summary overview section."
  ;; Add checks for nil dates in case processing failed partially
  (let* ((date-range-days (if (and bank-buddy-date-first bank-buddy-date-last)
                              (bank-buddy-days-between bank-buddy-date-first bank-buddy-date-last)
                            0))
         (total-txns (+ (gethash "under-10" bank-buddy-txn-size-dist 0)
                        (gethash "10-to-50" bank-buddy-txn-size-dist 0)
                        (gethash "50-to-100" bank-buddy-txn-size-dist 0)
                        (gethash "over-100" bank-buddy-txn-size-dist 0)))
         (total-spending 0)
         (months-count 0))

    ;; Calculate total spending
    (maphash (lambda (k v)
               (setq total-spending (+ total-spending v))
               (setq months-count (1+ months-count)))
             bank-buddy-monthly-totals)

    (insert "* Summary Overview\n\n")
    (insert (format "- *Total Transactions Analyzed:* %d\n" total-txns))
    (if (and bank-buddy-date-first bank-buddy-date-last)
        (insert (format "- *Date Range:* %s to %s (%d days)\n"
                        bank-buddy-date-first
                        bank-buddy-date-last
                        date-range-days))
      (insert "- *Date Range:* Could not be determined.\n"))
    (insert (format "- *Total Spending:* £%.2f\n" total-spending))
    (if (> date-range-days 0)
        (progn
          (insert (format "- *Average Daily Spending:* £%.2f\n"
                          (/ total-spending date-range-days)))
          (insert (format "- *Average Monthly Spending (approx):* £%.2f\n"
                          (/ total-spending (/ date-range-days 30.0)))))
      (insert "- *Average Spending:* Cannot calculate without valid date range.\n"))))


(defun bank-buddy-generate-transaction-size-distribution ()
  "Generate transaction size distribution section."
  (let* ((under-10 (gethash "under-10" bank-buddy-txn-size-dist 0))
         (to-50 (gethash "10-to-50" bank-buddy-txn-size-dist 0))
         (to-100 (gethash "50-to-100" bank-buddy-txn-size-dist 0))
         (over-100 (gethash "over-100" bank-buddy-txn-size-dist 0))
         (total (+ under-10 to-50 to-100 over-100)))

    (insert "\n* Transaction Size Distribution\n\n")
    (if (> total 0)
        (progn
          (insert (format "- *Under £10:* %d transactions (%.1f%%)\n"
                          under-10
                          (* 100.0 (/ (float under-10) total))))
          (insert (format "- *£10 to £50:* %d transactions (%.1f%%)\n"
                          to-50
                          (* 100.0 (/ (float to-50) total))))
          (insert (format "- *£50 to £100:* %d transactions (%.1f%%)\n"
                          to-100
                          (* 100.0 (/ (float to-100) total))))
          (insert (format "- *Over £100:* %d transactions (%.1f%%)\n"
                          over-100
                          (* 100.0 (/ (float over-100) total)))))
      (insert "No transaction data available for distribution.\n"))))


(defun bank-buddy-generate-top-spending-categories ()
  "Generate top spending categories section."
  (let ((categories '())
        (total-spending 0))

    ;; Sum up spending by category
    (maphash (lambda (key value)
               (let* ((parts (split-string key "-"))
                      ;; Category should be the last part (YYYY-MM-cat)
                      (category (if (> (length parts) 2) (nth (1- (length parts)) parts) "unknown"))
                      (existing (assoc category categories)))
                 (if existing
                     (setcdr existing (+ (cdr existing) value))
                   (push (cons category value) categories))))
             bank-buddy-cat-tot)

    ;; Calculate total spending
    (setq total-spending (apply '+ (mapcar 'cdr categories)))

    ;; Sort by amount (descending)
    (setq categories (sort categories (lambda (a b) (> (cdr a) (cdr b)))))

    (insert "\n* Top Spending Categories\n\n")
    (if (and categories (> total-spending 0))
        (let ((counter 1))
          (dolist (cat categories)
            (when (<= counter 10) ;; Only show top 10
              (let ((cat-name (cdr (assoc (car cat) bank-buddy-category-names))))
                (insert (format "%d. *%s:* £%.2f (%.1f%%)\n"
                                counter
                                (or cat-name (car cat))
                                (cdr cat)
                                (* 100.0 (/ (cdr cat) total-spending)))))
              (setq counter (1+ counter)))))
      (insert "No category spending data available.\n"))))

(defun bank-buddy-generate-top-merchants ()
  "Generate top merchants section."
  (let ((merchants-list '()))

    ;; Convert hash to list for sorting
    (maphash (lambda (k v) (push (cons k v) merchants-list))
             bank-buddy-merchants)

    ;; Sort by amount (descending)
    (setq merchants-list (sort merchants-list (lambda (a b) (> (cdr a) (cdr b)))))

    (insert "\n* Top 10 Merchants\n\n")
    (if merchants-list
        (let ((counter 1))
          (dolist (merchant merchants-list)
            (when (<= counter 10) ;; Only show top 10
              (insert (format "%d. *%s:* £%.2f\n"
                              counter
                              (car merchant)
                              (cdr merchant)))
              (setq counter (1+ counter)))))
      (insert "No merchant spending data available.\n"))))


(defun bank-buddy-generate-monthly-spending ()
  "Generate monthly spending patterns section."
  (let* ((months-list '())
         (year-months (mapcar #'identity (hash-table-keys bank-buddy-monthly-totals))) ; Get keys
         (total-spending 0)
         (month-count 0)
         highest-month lowest-month
         highest-amount lowest-amount)

    ;; Check if year-months is not empty before sorting
    (when year-months
      (setq year-months (sort year-months #'string<))

      ;; Calculate total and identify highest/lowest
      (dolist (month year-months)
        (let ((amount (gethash month bank-buddy-monthly-totals 0)))
          (push (cons month amount) months-list)
          (setq total-spending (+ total-spending amount))
          (setq month-count (1+ month-count))

          ;; Track highest and lowest
          (when (> amount 0) ; Only consider months with spending
            (if (or (not highest-amount) (> amount highest-amount))
                (setq highest-month month
                      highest-amount amount))
            (if (or (not lowest-amount) (< amount lowest-amount))
                (setq lowest-month month
                      lowest-amount amount))))))

    (insert "\n* Monthly Spending Patterns\n\n")
    (if (> month-count 0)
        (progn
          (if highest-month
              (insert (format "- *Highest Month:* %s (£%.2f)\n"
                              highest-month highest-amount))
            (insert "- *Highest Month:* N/A\n"))
          (if lowest-month
              (insert (format "- *Lowest Month:* %s (£%.2f)\n"
                              lowest-month lowest-amount))
            (insert "- *Lowest Month:* N/A\n"))
          (insert (format "- *Average Monthly Spending:* £%.2f\n\n"
                          (/ total-spending month-count)))

          ;; Monthly breakdown
          (insert "** Monthly Spending Breakdown\n")
          (insert "#+begin_src text\n") ; Use text for simpler formatting

          ;; Sort the list representation for output
          (setq months-list (sort months-list (lambda (a b) (string< (car a) (car b)))))
          (dolist (month-data months-list)
            (insert (format "%s: £%.2f\n" (car month-data) (cdr month-data))))

          (insert "#+end_src\n"))
      (insert "No monthly spending data available.\n"))))


(defun bank-buddy-generate-subscriptions ()
  "Generate recurring subscriptions section."
  (let ((subscriptions '())
        (monthly-total 0))

    ;; Find likely subscriptions based on recurrence patterns
    (maphash
     (lambda (key occurrences)
       (when (>= (length occurrences) bank-buddy-subscription-min-occurrences)
         (let* ((parts (split-string key "-"))
                ;; Combine name parts except the last (amount)
                (sub-name (mapconcat #'identity (butlast parts) "-"))
                (amount-str (car (last parts)))
                (amount (if (string-match-p "^[0-9.]+$" amount-str) (string-to-number amount-str) 0))
                (frequencies (bank-buddy-analyze-subscription-frequency occurrences)))
           (when (> amount 0) ; Only add if amount is valid
             (push (list sub-name amount frequencies) subscriptions)))))
     bank-buddy-subs)

    ;; Sort by amount (descending)
    (setq subscriptions (sort subscriptions (lambda (a b) (> (nth 1 a) (nth 1 b)))))

    ;; Calculate approximate monthly total
    (dolist (sub subscriptions)
      (let ((amount (nth 1 sub))
            (freq (nth 2 sub)))
        (cond
         ((string= freq "monthly")   (setq monthly-total (+ monthly-total amount)))
         ((string= freq "bi-weekly") (setq monthly-total (+ monthly-total (* amount 2))))
         ((string= freq "weekly")    (setq monthly-total (+ monthly-total (* amount 4))))
         ((string= freq "annual")    (setq monthly-total (+ monthly-total (/ amount 12.0))))
         ;; Irregular or unknown: treat as one-off monthly for estimation? Or ignore? Let's add it once.
         (t (setq monthly-total (+ monthly-total amount))))))

    (insert "\n* Recurring Subscriptions (Detected)\n\n")
    (insert (format "Estimated monthly cost from detected recurring payments: *£%.2f*\n"
                    monthly-total))
    (insert "(Note: Detection is based on pattern matching and frequency analysis, may not be exhaustive or perfectly accurate.)\n\n")

    (if subscriptions
        (let ((counter 1))
          (dolist (sub subscriptions)
            ;; Limit displayed subscriptions for brevity if needed
            ;; (when (<= counter 15)
            (let* ((name (nth 0 sub))
                   (amount (nth 1 sub))
                   (freq (nth 2 sub))
                   (frequency-text
                    (cond
                     ((string= freq "monthly") (format "£%.2f/month" amount))
                     ((string= freq "bi-weekly") (format "£%.2f/bi-weekly (approx £%.2f/month)" amount (* 2 amount)))
                     ((string= freq "weekly") (format "£%.2f/week (approx £%.2f/month)" amount (* 4 amount)))
                     ((string= freq "annual") (format "£%.2f/year (approx £%.2f/month)" amount (/ amount 12.0)))
                     (t (format "£%.2f (irregular/unknown frequency)" amount)))))
              (insert (format "%d. *%s:* %s\n" counter name frequency-text))
              (setq counter (1+ counter)))) ;;) ; Uncomment closing parens if limit uncommented
          )
      (insert "No potential recurring subscriptions detected meeting the criteria.\n"))))

(defun bank-buddy-analyze-subscription-frequency (occurrences)
  "Analyze OCCURRENCES of a transaction to determine likely subscription frequency."
  (let* ((sorted-occurrences
          (sort (copy-sequence occurrences)
                (lambda (a b) (string< (car a) (car b)))))
         (intervals '())
         (prev-date nil))

    ;; Calculate intervals between occurrences (in days)
    (dolist (occurrence sorted-occurrences)
      (let ((current-date (car occurrence)))
        (when (and prev-date
                   (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" current-date)
                   (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" prev-date))
          (condition-case nil ; Ignore errors from date parsing if format is odd
              (let ((days (bank-buddy-days-between prev-date current-date)))
                (when (> days 0) ; Ignore same-day duplicates
                  (push days intervals)))
            (error nil))) ; Skip interval calculation on error
        (setq prev-date current-date))) ; Update previous date

    ;; Determine likely frequency pattern based on average interval +/- tolerance
    (if (< (length intervals) 2) ; Need at least 2 intervals (3 occurrences) for good guess
        "insufficient data"
      (let* ((avg-interval (/ (apply '+ intervals) (float (length intervals))))
             ;; Define tolerances around common periods
             (weekly-low 5) (weekly-high 10)
             (biweekly-low 11) (biweekly-high 20)
             (monthly-low 25) (monthly-high 35)
             (annual-low 350) (annual-high 380))
        (cond
         ((and (>= avg-interval weekly-low) (<= avg-interval weekly-high)) "weekly")
         ((and (>= avg-interval biweekly-low) (<= avg-interval biweekly-high)) "bi-weekly")
         ((and (>= avg-interval monthly-low) (<= avg-interval monthly-high)) "monthly")
         ((and (>= avg-interval annual-low) (<= avg-interval annual-high)) "annual")
         (t "irregular"))))))


(defun bank-buddy-generate-plots ()
  "Generate org-plot visualizations of financial data."
  (insert "\n* Data Visualizations\n\n")
  (require 'org-plot) ; Ensure org-plot is loaded

  ;; Monthly spending plot
  (insert "** Monthly Spending Trend\n\n")
  (insert "This plot shows your spending over time.\n\n")
  (insert "#+PLOT: title:\"Monthly Spending Trend\" ind:1 deps:(2) type:2d with:linespoints set:\"grid\" set:\"ylabel 'Spending (£)'\" set:\"xdata time\" set:\"timefmt '%Y-%m'\" set:\"format x '%b\\n%Y'\" set:\"xtics rotate by -45\"\n")
  (insert "#+NAME: monthly-spending-trend\n")
  
  (insert "| Month    | Spending |\n")
  (insert "|----------+----------|\n")
  (let ((month-list '()))
    (maphash (lambda (month amount) (push (cons month amount) month-list)) bank-buddy-monthly-totals)
    (setq month-list (sort month-list (lambda (a b) (string< (car a) (car b)))))
    (if month-list
        (dolist (month-data month-list)
          (insert (format "| %s | %.2f |\n" (car month-data) (cdr month-data))))
      (insert "| No Data  | 0.00     |\n")))
  (insert "\n\n")

  (insert "#+begin_src gnuplot :var data=monthly-spending-trend :file financial-report--monthly-spending-trend.png :execute_on_open t :results file\n")
  (insert "set terminal png size 800,600\n")
  (insert "set style data histogram\n")
  (insert "set style fill solid\n")
  (insert "set boxwidth 0.8\n")
  (insert "set xtics rotate by -45\n")
  (insert "set ylabel \"Amount\"\n")
  (insert "set title \"Monthly Spending Trend\"\n")
  (insert "plot data using 2:xtic(1) with boxes title \"Amount\"\n")
  (insert "#+end_src\n\n\n")
  
  (insert "#+ATTR_ORG: :width 600\n")
  (insert "#+RESULTS:\n")
  (insert "[[file:financial-report--monthly-spending-trend.png]]\n\n")
  
  ;; Spending by category plot
  (insert "** Top Spending Categories (Histogram)\n\n")
  (insert "#+PLOT: title:\"Top Spending Categories\" ind:1 deps:(2) type:histogram with:histograms set:\"style fill solid 0.8\" set:\"grid\" set:\"ylabel 'Amount (£)'\" set:\"xtic(1)\" set:\"xtics rotate by -45\"\n")
  (insert "#+NAME: top-spending-categories\n")
  (insert "| Category        | Amount |\n")
  (insert "|-----------------+--------|\n")
  (let ((categories '()) (total-spending 0))
    (maphash (lambda (key value)
               (let* ((parts (split-string key "-"))
                      (category (if (> (length parts) 2) (nth (1- (length parts)) parts) "unknown"))
                      (existing (assoc category categories)))
                 (if existing (setcdr existing (+ (cdr existing) value)) (push (cons category value) categories))))
             bank-buddy-cat-tot)
    (setq categories (sort categories (lambda (a b) (> (cdr a) (cdr b)))))
    (if categories
        (dotimes (i (min 10 (length categories)))
          (let* ((cat (nth i categories))
                 (cat-name (cdr (assoc (car cat) bank-buddy-category-names))))
            (insert (format "| %s | %.2f |\n" (or cat-name (car cat)) (cdr cat)))))
      (insert "| No Data         | 0.00   |\n")))
  (insert "\n\n")

  (insert "#+begin_src gnuplot :var data=top-spending-categories :file financial-report--top-spending-categories.png :execute_on_open t :results file\n")
  (insert "set terminal png size 800,600\n")
  (insert "set style data histogram\n")
  (insert "set style fill solid\n")
  (insert "set boxwidth 0.8\n")
  (insert "set xtics rotate by -45\n")
  (insert "set ylabel \"Amount\"\n")
  (insert "set title \"Top Spending Categories\"\n")
  (insert "plot data using 2:xtic(1) with boxes title \"Amount\"\n")
  (insert "#+end_src\n\n\n")
  
  (insert "#+ATTR_ORG: :width 600\n")
  (insert "#+RESULTS:\n")
  (insert "[[file:financial-report--top-spending-categories.png]]\n\n")

  ;; Transaction size distribution plot
  (insert "** Transaction Size Distribution (Pie Chart)\n\n")
  (insert "#+PLOT: title:\"Transaction Size Distribution\" ind:1 deps:(2) type:pie with:labels\n")
  (insert "#+NAME: transaction-size-distribution\n")
  (insert "| Range         | Count |\n")
  (insert "|---------------+-------|\n")
  (let* ((under-10 (gethash "under-10" bank-buddy-txn-size-dist 0))
         (to-50 (gethash "10-to-50" bank-buddy-txn-size-dist 0))
         (to-100 (gethash "50-to-100" bank-buddy-txn-size-dist 0))
         (over-100 (gethash "over-100" bank-buddy-txn-size-dist 0))
         (total (+ under-10 to-50 to-100 over-100)))
    (if (> total 0)
        (progn
          (insert (format "| Under £10     | %d |\n" under-10))
          (insert (format "| £10 to £50    | %d |\n" to-50))
          (insert (format "| £50 to £100   | %d |\n" to-100))
          (insert (format "| Over £100     | %d |\n" over-100)))
      (insert "| No Data       | 0     |\n")))
  (insert "\n\n")

  (insert "#+begin_src gnuplot :var data=top-spending-categories :file financial-report--transaction-size-distribution.png :execute_on_open t :results file\n")
  (insert "set terminal png size 800,600\n")
  (insert "set style data histogram\n")
  (insert "set style fill solid\n")
  (insert "set boxwidth 0.8\n")
  (insert "set xtics rotate by -45\n")
  (insert "set ylabel \"Amount\"\n")
  (insert "set title \"Transaction Size Distribution\"\n")
  (insert "plot data using 2:xtic(1) with boxes title \"Amount\"\n")
  (insert "#+end_src\n\n\n")
  
  (insert "#+ATTR_ORG: :width 600\n")
  (insert "#+RESULTS:\n")
  (insert "[[file:financial-report--transaction-size-distribution.png]]\n\n")
  
  ;; Subscription costs plot (Simplified - plotting actual occurrences might be better but complex)
  ;; Let's plot the estimated monthly cost based on detected subs
  (insert "** Estimated Monthly Subscription Cost Trend\n\n")
  (insert "Estimated total cost per month based on detected recurring payments active during that month.\n\n")
  (insert "#+PLOT: title:\"Estimated Monthly Subscription Costs\" ind:1 deps:(2) type:2d with:linespoints set:\"grid\" set:\"ylabel 'Estimated Cost (£)'\" set:\"xdata time\" set:\"timefmt '%Y-%m'\" set:\"format x '%b\\n%Y'\" set:\"xtics rotate by -45\"\n")
  (insert "#+NAME: monthly-subscription-costs\n")
  (insert "| Month    | Est. Cost |\n")
  (insert "|----------+-----------|\n")
  (let ((monthly-costs (make-hash-table :test 'equal)))
    ;; Iterate through all detected subs and their occurrences
    (maphash
     (lambda (key occurrences)
       (when (>= (length occurrences) bank-buddy-subscription-min-occurrences)
         (let* ((parts (split-string key "-"))
                (amount-str (car (last parts)))
                (amount (if (string-match-p "^[0-9.]+$" amount-str) (string-to-number amount-str) 0))
                (freq (bank-buddy-analyze-subscription-frequency occurrences))
                (monthly-equiv-cost
                 (cond ((string= freq "monthly") amount)
                       ((string= freq "bi-weekly") (* amount 2.0))
                       ((string= freq "weekly") (* amount 4.0)) ; Approximation
                       ((string= freq "annual") (/ amount 12.0))
                       (t 0.0)))) ; Ignore irregular/unknown for monthly plot
           (when (> monthly-equiv-cost 0)
             (dolist (occurrence occurrences)
               (let* ((date (car occurrence))
                      (month (if (and date (>= (length date) 7)) (substring date 0 7) nil)))
                 (when month
                   (puthash month (+ (gethash month monthly-costs 0.0) monthly-equiv-cost) monthly-costs))))))))
     bank-buddy-subs)

    ;; Generate table rows
    (let ((month-list '()))
      (maphash (lambda (month cost) (push (cons month cost) month-list)) monthly-costs)
      (setq month-list (sort month-list (lambda (a b) (string< (car a) (car b)))))
      (if month-list
          (dolist (month-data month-list)
            (insert (format "| %s | %.2f |\n" (car month-data) (cdr month-data))))
        (insert "| No Data  | 0.00      |\n"))) )
  (insert "\n\n")

  (insert "#+begin_src gnuplot :var data=monthly-subscription-costs :file financial-report--monthly-subscription-costs.png :execute_on_open t :results file\n")
  (insert "set terminal png size 800,600\n")
  (insert "set style data histogram\n")
  (insert "set style fill solid\n")
  (insert "set boxwidth 0.8\n")
  (insert "set xtics rotate by -45\n")
  (insert "set ylabel \"Amount\"\n")
  (insert "set title \"Monthly Subscription Costs\"\n")
  (insert "plot data using 2:xtic(1) with boxes title \"Amount\"\n")
  (insert "#+end_src\n\n\n")
  
  (insert "#+ATTR_ORG: :width 600\n")
  (insert "#+RESULTS:\n")
  (insert "[[file:financial-report--monthly-subscription-costs.png]]\n\n")



  )


;; --- Main Entry Point ---

;;;###autoload
(defun bank-buddy-generate-report (csv-file output-file)
  "Generate financial report from CSV-FILE asynchronously and save to OUTPUT-FILE."
  (interactive "fInput CSV file: \nFOutput Org file: ")

  ;; Ensure async is available
  (unless (fboundp 'async-start)
    (error "async.el library not found. Please install and load it."))

  ;; Ensure the CSV file exists and is readable
  (unless (file-readable-p csv-file)
    (error "Cannot read CSV file: %s" csv-file))

  ;; Ensure the file paths are absolute
  (setq csv-file (expand-file-name csv-file))
  (setq output-file (expand-file-name output-file))
  
  ;; Inform the user that processing has started
  (bank-buddy-show-progress (format "Starting analysis of %s..." (file-name-nondirectory csv-file)))

  ;; Start the asynchronous task
  (async-start
   ;; The worker function
   `(lambda ()
      ;; Set up the environment for the async process
      (setq load-path ',load-path)
      
      ;; Load minimal required libraries
      (require 'cl-lib)
      
      ;; Load the bank-buddy package
      (let ((bank-buddy-file ,(or load-file-name 
                                  (locate-library "bank-buddy")
                                  (buffer-file-name))))
        (when bank-buddy-file
          (load-file bank-buddy-file))
        
        ;; Pass all needed variables to the worker
        (let ((bank-buddy-exclude-large-txns ,bank-buddy-exclude-large-txns)
              (bank-buddy-large-txn-threshold ,bank-buddy-large-txn-threshold)
              (bank-buddy-subscription-min-occurrences ,bank-buddy-subscription-min-occurrences)
              (bank-buddy-cat-list-defines ',bank-buddy-cat-list-defines)
              (bank-buddy-subscription-patterns ',bank-buddy-subscription-patterns)
              (bank-buddy-category-names ',bank-buddy-category-names))
          
          ;; Call the worker function and include file paths in the result
          (let ((worker-result (bank-buddy--process-csv-async-worker ,csv-file)))
            ;; Add the file paths to the result plist
            (plist-put worker-result :csv-file ,csv-file)
            (plist-put worker-result :output-file ,output-file)
            worker-result))))
   
   ;; The callback function that uses file paths from the result
   (lambda (result)
     ;; Extract file paths from the result
     (let ((csv-file (plist-get result :csv-file))
           (output-file (plist-get result :output-file))
           (worker-err (plist-get result :error)))
       
       (if worker-err
           ;; Handle worker-reported error
           (progn
             (message "Bank Buddy: Background processing failed: %s" worker-err)
             (display-warning 'bank-buddy (format "Background processing failed: %s" worker-err) :error))

         ;; Process success (no worker error reported)
         (progn
           (bank-buddy-show-progress "Generating report..." t)

           ;; Clear previous global data
           (clrhash bank-buddy-cat-tot)
           (clrhash bank-buddy-merchants)
           (clrhash bank-buddy-monthly-totals)
           (clrhash bank-buddy-txn-size-dist)
           (clrhash bank-buddy-subs)
           (setq bank-buddy-unmatched-transactions '()) ; Clear previous unmatched list
           
           (setq bank-buddy-date-first nil)
           (setq bank-buddy-date-last nil)
           ;; Populate globals from the returned plist
           (setq bank-buddy-date-first (plist-get result :date-first))
           (setq bank-buddy-date-last (plist-get result :date-last))
           
           (when (hash-table-p (plist-get result :cat-tot))
             (maphash (lambda (k v) (puthash k v bank-buddy-cat-tot)) (plist-get result :cat-tot)))
           (when (hash-table-p (plist-get result :merchants))
             (maphash (lambda (k v) (puthash k v bank-buddy-merchants)) (plist-get result :merchants)))
           (when (hash-table-p (plist-get result :monthly-totals))
             (maphash (lambda (k v) (puthash k v bank-buddy-monthly-totals)) (plist-get result :monthly-totals)))
           (when (hash-table-p (plist-get result :txn-size-dist))
             (maphash (lambda (k v) (puthash k v bank-buddy-txn-size-dist)) (plist-get result :txn-size-dist)))
           (when (hash-table-p (plist-get result :subs))
             (maphash (lambda (k v) (puthash k v bank-buddy-subs)) (plist-get result :subs)))
             
           ;; Get the unmatched transactions list
           (setq bank-buddy-unmatched-transactions (plist-get result :unmatched-transactions))
           
           ;; Generate the report content in a temp buffer
           (with-temp-buffer
             (org-mode)
             (insert "#+title: Financial Report (Bank Buddy)\n")
             (insert (format "#+subtitle: Data from %s\n" (file-name-nondirectory csv-file)))
             (insert (format "#+date: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
             (insert "#+options: toc:2 num:nil\n")
             (insert "#+startup: inlineimages showall\n\n")
             (bank-buddy-generate-summary-overview)
             (bank-buddy-generate-transaction-size-distribution)
             (bank-buddy-generate-top-spending-categories)
             (bank-buddy-generate-top-merchants)
             (bank-buddy-generate-monthly-spending)
             (bank-buddy-generate-subscriptions)
             (bank-buddy-generate-unmatched-transactions) ; Add the new section
             (bank-buddy-generate-plots)
             (write-region (point-min) (point-max) output-file nil 'quiet))
           
           (bank-buddy-show-progress (format "Report generated successfully: %s" output-file) t)
           
           (when (yes-or-no-p (format "Open generated report %s now?" output-file))
             (find-file output-file)))))))
  )

(defun bank-buddy-view-unmatched-transactions ()
  "View the list of transactions that weren't matched by specific patterns."
  (interactive)
  (if (not bank-buddy-unmatched-transactions)
      (message "No unmatched transactions available. Generate a report first.")
    (with-current-buffer (get-buffer-create "*Bank Buddy Unmatched*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* Unmatched Transactions\n\n")
        (insert "The following transactions were only matched by the catch-all pattern (\".*\").\n")
        (insert "You may want to add specific patterns for these in `bank-buddy-cat-list-defines`.\n\n")
        (insert "Copy these to use for your regex development:\n\n")
        (insert "#+begin_src elisp\n")
        (insert ";; Add these patterns to bank-buddy-cat-list-defines\n")
        (dolist (txn (sort (copy-sequence bank-buddy-unmatched-transactions) #'string<))
          (insert (format "(\"%s\" \"CATEGORY\") ;; Replace CATEGORY with appropriate code\n" txn)))
        (insert "#+end_src\n\n")
        (insert "Raw transaction names for reference:\n\n")
        (insert "#+begin_src text\n")
        (dolist (txn (sort (copy-sequence bank-buddy-unmatched-transactions) #'string<))
          (insert (format "%s\n" txn)))
        (insert "#+end_src\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; Utility function for development/debugging (no change needed)
(defun bank-buddy-debug-info ()
  "Display debug information about current data state."
  (interactive)
  (message "Bank Buddy Debug Info:")
  (message " Dates: %s to %s" bank-buddy-date-first bank-buddy-date-last)
  (message " Cat Totals: %d entries" (hash-table-count bank-buddy-cat-tot))
  (message " Merchants: %d entries" (hash-table-count bank-buddy-merchants))
  (message " Monthly Totals: %d entries" (hash-table-count bank-buddy-monthly-totals))
  (message " Txn Size Dist: %d entries" (hash-table-count bank-buddy-txn-size-dist))
  (message " Subscriptions: %d entries" (hash-table-count bank-buddy-subs))
  ;; Optionally display some sample data if needed
  )

(provide 'bank-buddy)

;;; bank-buddy.el ends here
