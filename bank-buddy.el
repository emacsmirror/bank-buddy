;;; bank-buddy.el --- Enhanced financial analysis and reporting for Emacs

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your-email@example.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (csv "0.5"))
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
;; The package reads CSV bank statement data, categorizes transactions, and
;; generates detailed reports in Org-mode format.

;;; Code:

(require 'csv)
(require 'cl-lib)

;; Variables and data structures
(defvar bank-buddy-payments '()
  "List of parsed payment transactions.")

(defvar bank-buddy-cat-tot (make-hash-table :test 'equal)
  "Hash table storing category totals.")

(defvar bank-buddy-merchants (make-hash-table :test 'equal)
  "Hash table storing merchant totals.")

(defvar bank-buddy-monthly-totals (make-hash-table :test 'equal)
  "Hash table storing monthly spending totals.")

(defvar bank-buddy-txn-size-dist (make-hash-table :test 'equal)
  "Hash table for tracking transaction size distribution.")

(defvar bank-buddy-subs (make-hash-table :test 'equal)
  "Hash table for tracking potential subscriptions.")

(defvar bank-buddy-date-first nil
  "First transaction date.")

(defvar bank-buddy-date-last nil
  "Last transaction date.")

;; Customization options
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

;; Original category mappings
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

;; Human-readable category names for reporting
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

;; Subscription-related patterns with descriptive names
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

;; Core Processing Functions

(defun bank-buddy-parse-csv-file (file)
  "Parse CSV FILE and store payments in bank-buddy-payments."
  (with-temp-buffer
    (insert-file-contents file)
    (setq bank-buddy-payments (csv-parse-buffer t))))

(defun bank-buddy-calculate-date-range ()
  "Calculate the first and last dates from transactions."
  (let ((dates (mapcar (lambda (payment)
                         (cdr (nth 0 payment)))
                       bank-buddy-payments)))
    (setq bank-buddy-date-first (car (sort (copy-sequence dates) 'string<)))
    (setq bank-buddy-date-last (car (sort (copy-sequence dates) 'string>)))))

(defun bank-buddy-days-between (date1 date2)
  "Calculate days between DATE1 and DATE2 in YYYY-MM-DD format."
  (let ((time1 (date-to-time date1))
        (time2 (date-to-time date2)))
    (floor (/ (float-time (time-subtract time2 time1)) 86400))))

(defun bank-buddy-categorize-payment (name debit month date)
  "Categorize payment based on NAME, DEBIT amount, MONTH and DATE."
  (let ((category-found nil)
        (split-key nil)
        (merchant (replace-regexp-in-string " .*" "" name)))
    
    ;; Find category
    (cl-loop for category in bank-buddy-cat-list-defines
             when (string-match-p (nth 0 category) name)
             do (setq category-found (nth 1 category))
             and return t)
    
    ;; Update category totals
    (setq split-key (concat month "-" category-found))
    (puthash split-key 
             (+ (gethash split-key bank-buddy-cat-tot 0) debit) 
             bank-buddy-cat-tot)

    ;; Update monthly totals
    (puthash month
             (+ (gethash month bank-buddy-monthly-totals 0) debit)
             bank-buddy-monthly-totals)
    
    ;; Update merchant totals
    (puthash merchant
             (+ (gethash merchant bank-buddy-merchants 0) debit)
             bank-buddy-merchants)
    
    ;; Update transaction size distribution
    (cond
     ((<= debit 10)
      (puthash "under-10" (1+ (gethash "under-10" bank-buddy-txn-size-dist 0)) bank-buddy-txn-size-dist))
     ((<= debit 50)
      (puthash "10-to-50" (1+ (gethash "10-to-50" bank-buddy-txn-size-dist 0)) bank-buddy-txn-size-dist))
     ((<= debit 100)
      (puthash "50-to-100" (1+ (gethash "50-to-100" bank-buddy-txn-size-dist 0)) bank-buddy-txn-size-dist))
     (t
      (puthash "over-100" (1+ (gethash "over-100" bank-buddy-txn-size-dist 0)) bank-buddy-txn-size-dist)))
    
    ;; Track potential subscriptions
    (cl-loop for (pattern . sub-name) in bank-buddy-subscription-patterns
             when (string-match-p pattern (upcase name))
             do (let* ((sub-key (concat sub-name "-" (format "%.2f" debit)))
                       (dates (gethash sub-key bank-buddy-subs nil)))
                  (puthash sub-key 
                           (cons (cons date debit) dates)
                           bank-buddy-subs)))
    
    ;; Return the assigned category
    category-found))

(defun bank-buddy-process-payments ()
  "Process all payments and calculate totals."
  ;; Clear any previous data
  (clrhash bank-buddy-cat-tot)
  (clrhash bank-buddy-merchants)
  (clrhash bank-buddy-monthly-totals)
  (clrhash bank-buddy-txn-size-dist)
  (clrhash bank-buddy-subs)
  
  ;; Calculate date range
  (bank-buddy-calculate-date-range)
  
  ;; Process all payments
  (let ((total-transactions 0)
        (total-amount 0))
    (dolist (payment bank-buddy-payments)
      (let* ((date (cdr (nth 0 payment)))
             (month (substring date 0 7)) ;; YYYY-MM
             (description (cdr (nth 1 payment)))
             (debit-str (cdr (nth 2 payment)))
             (debit (if (string= debit-str "")
                        0
                      (string-to-number debit-str))))
        
        ;; Skip large transactions if configured
        (when (or (not bank-buddy-exclude-large-txns)
                  (< debit bank-buddy-large-txn-threshold))
          (when (> debit 0) ;; Only count positive debits
            (bank-buddy-categorize-payment 
             (replace-regexp-in-string " " "-" description)
             debit month date)
            (setq total-transactions (1+ total-transactions))
            (setq total-amount (+ total-amount debit))))))))

;; Reporting Functions

(defun bank-buddy-generate-summary-overview ()
  "Generate summary overview section."
  (let* ((total-txns (+ (gethash "under-10" bank-buddy-txn-size-dist 0)
                        (gethash "10-to-50" bank-buddy-txn-size-dist 0)
                        (gethash "50-to-100" bank-buddy-txn-size-dist 0)
                        (gethash "over-100" bank-buddy-txn-size-dist 0)))
         (date-range-days (bank-buddy-days-between 
                          bank-buddy-date-first
                          bank-buddy-date-last))
         (total-spending 0)
         (months-count 0))
    
    ;; Calculate total spending
    (maphash (lambda (k v) 
               (setq total-spending (+ total-spending v))
               (setq months-count (1+ months-count)))
             bank-buddy-monthly-totals)
    
    (insert "* Summary Overview\n\n")
    (insert (format "- *Total Transactions:* %d\n" total-txns))
    (insert (format "- *Date Range:* %s to %s (%d days)\n" 
                    bank-buddy-date-first
                    bank-buddy-date-last
                    date-range-days))
    (insert (format "- *Total Spending:* £%.2f\n" total-spending))
    (insert (format "- *Average Daily Spending:* £%.2f\n" 
                    (/ total-spending date-range-days)))
    (insert (format "- *Average Monthly Spending:* £%.2f\n" 
                    (/ total-spending (/ date-range-days 30.0))))))

(defun bank-buddy-generate-transaction-size-distribution ()
  "Generate transaction size distribution section."
  (let* ((under-10 (gethash "under-10" bank-buddy-txn-size-dist 0))
         (to-50 (gethash "10-to-50" bank-buddy-txn-size-dist 0))
         (to-100 (gethash "50-to-100" bank-buddy-txn-size-dist 0))
         (over-100 (gethash "over-100" bank-buddy-txn-size-dist 0))
         (total (+ under-10 to-50 to-100 over-100)))
    
    (insert "\n* Transaction Size Distribution\n\n")
    (insert (format "- *Under £10:* %d transactions (%.1f%%)\n" 
                    under-10 
                    ;; Fix: Convert to float before division to avoid integer division
                    (* 100.0 (/ (float under-10) total))))
    (insert (format "- *£10 to £50:* %d transactions (%.1f%%)\n" 
                    to-50 
                    (* 100.0 (/ (float to-50) total))))
    (insert (format "- *£50 to £100:* %d transactions (%.1f%%)\n" 
                    to-100 
                    (* 100.0 (/ (float to-100) total))))
    (insert (format "- *Over £100:* %d transactions (%.1f%%)\n" 
                    over-100 
                    (* 100.0 (/ (float over-100) total))))))

(defun bank-buddy-generate-top-spending-categories ()
  "Generate top spending categories section."
  (let ((categories '())
        (total-spending 0))
    
    ;; Sum up spending by category
    (maphash (lambda (key value)
               (let* ((parts (split-string key "-"))
                      ;; Fix: The category is the last part (parts are "YYYY" "MM" "category")
                      (category (nth (1- (length parts)) parts))
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
    (let ((counter 1))
      (dolist (cat categories)
        (when (<= counter 10) ;; Only show top 10
          (let ((cat-name (cdr (assoc (car cat) bank-buddy-category-names))))
            (insert (format "%d. *%s:* £%.2f (%.1f%%)\n"
                            counter
                            (or cat-name (car cat))
                            (cdr cat)
                            (* 100.0 (/ (cdr cat) total-spending)))))
          (setq counter (1+ counter)))))))

(defun bank-buddy-generate-top-merchants ()
  "Generate top merchants section."
  (let ((merchants-list '()))
    
    ;; Convert hash to list for sorting
    (maphash (lambda (k v) (push (cons k v) merchants-list)) 
             bank-buddy-merchants)
    
    ;; Sort by amount (descending)
    (setq merchants-list (sort merchants-list (lambda (a b) (> (cdr a) (cdr b)))))
    
    (insert "\n* Top 10 Merchants\n\n")
    (let ((counter 1))
      (dolist (merchant merchants-list)
        (when (<= counter 10) ;; Only show top 10
          (insert (format "%d. *%s:* £%.2f\n"
                          counter
                          (car merchant)
                          (cdr merchant)))
          (setq counter (1+ counter)))))))

(defun bank-buddy-generate-monthly-spending ()
  "Generate monthly spending patterns section."
  (let* ((months-list '())
         (year-months (sort (mapcar (lambda (month-data) 
                                       month-data)
                                     (hash-table-keys bank-buddy-monthly-totals))
                               'string<))
         (total-spending 0)
         (month-count 0)
         highest-month lowest-month
         highest-amount lowest-amount)
    
    ;; Calculate total and identify highest/lowest
    (dolist (month year-months)
      (let ((amount (gethash month bank-buddy-monthly-totals 0)))
        (push (cons month amount) months-list)
        (setq total-spending (+ total-spending amount))
        (setq month-count (1+ month-count))
        
        ;; Track highest and lowest
        (if (or (not highest-amount) (> amount highest-amount))
            (setq highest-month month
                  highest-amount amount))
        
        (if (or (not lowest-amount) (< amount lowest-amount))
            (setq lowest-month month
                  lowest-amount amount))))
    
    (insert "\n* Monthly Spending Patterns\n\n")
    (insert (format "- *Highest Month:* %s (£%.2f)\n" 
                    highest-month highest-amount))
    (insert (format "- *Lowest Month:* %s (£%.2f)\n" 
                    lowest-month lowest-amount))
    (insert (format "- *Average Monthly Spending:* £%.2f\n\n" 
                    (/ total-spending month-count)))
    
    ;; Monthly breakdown
    (insert "** Monthly Spending Breakdown\n")
    (insert "#+begin_src \n")
    
    ;; Group by year
    (let* ((by-year (make-hash-table :test 'equal))
           (year-order '()))
      
      ;; Group months by year
      (dolist (month-data (sort months-list (lambda (a b) (string< (car a) (car b)))))
        (let* ((month (car month-data))
               (year (substring month 0 4))
               (month-num (substring month 5 7))
               (amount (cdr month-data))
               (month-entry (cons month-num amount))
               (year-months (gethash year by-year nil)))
          
          ;; Add year to ordered list if not present
          (unless (member year year-order)
            (push year year-order))
          
          ;; Add month data to year
          (if year-months
              (puthash year (cons month-entry year-months) by-year)
            (puthash year (list month-entry) by-year))))
      
      ;; Print in columns by year
      (setq year-order (sort year-order 'string<))
      (let ((months-per-year (/ 12 (length year-order)))
            (current-month 0))
        
        (dotimes (month-index 12)
          (dolist (year year-order)
            (let* ((year-data (gethash year by-year nil))
                   (month-num (format "%02d" (1+ month-index)))
                   (month-data (assoc month-num year-data))
                   (month-amount (if month-data (cdr month-data) 0)))
              
              (insert (format "%s-%s: £%.2f    " 
                              year month-num month-amount))
              
              (setq current-month (1+ current-month))
              (when (= current-month (length year-order))
                (insert "\n")
                (setq current-month 0))))))
      
      (insert "#+end_src\n"))))

(defun bank-buddy-generate-subscriptions ()
  "Generate recurring subscriptions section."
  (let ((subscriptions '())
        (monthly-total 0))
    
    ;; Find likely subscriptions based on recurrence patterns
    (maphash
     (lambda (key occurrences)
       (when (>= (length occurrences) bank-buddy-subscription-min-occurrences)
         (let* ((parts (split-string key "-"))
                (sub-name (nth 0 parts)) ;; FIXED: Use parts instead of key
                (amount (car (last parts)))
                (frequencies (bank-buddy-analyze-subscription-frequency occurrences)))
           (push (list sub-name (string-to-number amount) frequencies) subscriptions))))
     bank-buddy-subs)

    ;; Sort by amount (descending)
    (setq subscriptions (sort subscriptions (lambda (a b) (> (nth 1 a) (nth 1 b)))))
    
    ;; Calculate approximate monthly total
    (dolist (sub subscriptions)
      (let ((freq (nth 2 sub)))
        (cond 
         ((string= freq "monthly")
          (setq monthly-total (+ monthly-total (nth 1 sub))))
         ((string= freq "bi-weekly")
          (setq monthly-total (+ monthly-total (* (nth 1 sub) 2))))
         ((string= freq "weekly")
          (setq monthly-total (+ monthly-total (* (nth 1 sub) 4))))
         ((string= freq "annual")
          (setq monthly-total (+ monthly-total (/ (nth 1 sub) 12))))
         (t (setq monthly-total (+ monthly-total (nth 1 sub)))))))
    
    (insert "\n* Recurring Subscriptions\n\n")
    (insert (format "Your estimated monthly subscription cost is approximately *£%.2f*\n\n" 
                    monthly-total))
    
    (let ((counter 1))
      (dolist (sub subscriptions)
        (when (<= counter 10) ;; Limit to top 10
          (let ((frequency-text 
                 (cond
                  ((string= (nth 2 sub) "monthly") "/month")
                  ((string= (nth 2 sub) "bi-weekly") "/month (bi-weekly charges)")
                  ((string= (nth 2 sub) "weekly") "/month (weekly charges)")
                  ((string= (nth 2 sub) "annual") "/month (annual charge)")
                  (t " (frequency undetermined)"))))
            (insert (format "%d. *%s:* £%.2f%s\n"
                            counter
                            (nth 0 sub)
                            (nth 1 sub)
                            frequency-text)))
          (setq counter (1+ counter)))))))

(defun bank-buddy-analyze-subscription-frequency (occurrences)
  "Analyze OCCURRENCES of a transaction to determine likely subscription frequency."
  (let* ((sorted-occurrences 
          (sort (copy-sequence occurrences) 
                (lambda (a b) (string< (car a) (car b)))))
         (intervals '())
         (prev-date nil))
    
    ;; Calculate intervals between occurrences
    (dolist (occurrence sorted-occurrences)
      (when prev-date
        (let ((days (bank-buddy-days-between prev-date (car occurrence))))
          (push days intervals)))
      (setq prev-date (car occurrence)))
    
    ;; Determine likely frequency pattern
    (when intervals
      (let ((avg-interval (/ (apply '+ intervals) (length intervals))))
        (cond
         ((<= avg-interval 10) "weekly")
         ((<= avg-interval 20) "bi-weekly")
         ((<= avg-interval 40) "monthly")
         ((>= avg-interval 300) "annual")
         (t "irregular"))))))

(defun bank-buddy-generate-plots ()
  "Generate org-plot visualizations of financial data."
  (insert "\n* Data Visualizations\n\n")
  
  ;; Monthly spending plot with time-based x-axis
  (insert "** Monthly Spending Trend\n\n")
  (insert "This plot shows your spending over time. Each point represents a month's total spending.\n\n")
  
  ;; Use timecolumn format for proper time-based plotting
  (insert "#+PLOT: title:\"Monthly Spending Trend\" ind:1 deps:(2) type:2d with:linespoints \
set:\"grid\" set:\"ylabel 'Spending (£)'\" set:\"xdata time\" set:\"timefmt '%Y-%m'\" \
set:\"format x '%b\\n%Y'\" set:\"xtics rotate by -45\"\n")
  
  (insert "| Month | Spending |\n")
  (insert "|-------+----------|\n")
  
  ;; Sort months chronologically
  (let* ((month-list '()))
    
    ;; Convert hash to list for sorting
    (maphash (lambda (month amount) 
               (push (cons month amount) month-list))
             bank-buddy-monthly-totals)
    
    ;; Sort by month chronologically
    (setq month-list (sort month-list (lambda (a b) (string< (car a) (car b)))))
    
    ;; Generate table rows
    (dolist (month-data month-list)
      (insert (format "| %s | %.2f |\n" 
                     (car month-data) (cdr month-data)))))
  
  (insert "\n\n")
  
  ;; Spending by category plot
  (insert "** Top Spending Categories\n\n")
  (insert "#+PLOT: title:\"Top Spending Categories\" ind:1 deps:(2) type:histogram with:histogram \
set:\"style fill solid 0.8\" set:\"grid\" set:\"ylabel 'Amount (£)'\" set:\"xtic(1)\" \
set:\"xtics rotate by -45\"\n")
  
  (insert "| Category | Amount |\n")
  (insert "|---------+--------|\n")
  
  ;; Generate category data
  (let ((categories '()))
    
    ;; Sum up spending by category
    (maphash (lambda (key value)
               (let* ((parts (split-string key "-"))
                      (category (nth (1- (length parts)) parts))
                      (existing (assoc category categories)))
                 (if existing
                     (setcdr existing (+ (cdr existing) value))
                   (push (cons category value) categories))))
             bank-buddy-cat-tot)
    
    ;; Sort by amount (descending)
    (setq categories (sort categories (lambda (a b) (> (cdr a) (cdr b)))))
    
    ;; Generate table rows (top 10 only)
    (dotimes (i (min 10 (length categories)))
      (let* ((cat (nth i categories))
             (cat-name (cdr (assoc (car cat) bank-buddy-category-names))))
        (insert (format "| %s | %.2f |\n" 
                       (or cat-name (car cat))
                       (cdr cat))))))
  
  (insert "\n\n")
  
  ;; Transaction size distribution plot
  (insert "** Transaction Size Distribution\n\n")
  (insert "#+PLOT: title:\"Transaction Size Distribution\" ind:1 deps:(2) type:pie with:labels\n")
  (insert "| Category | Count |\n")
  (insert "|----------+-------|\n")
  
  (let* ((under-10 (gethash "under-10" bank-buddy-txn-size-dist 0))
         (to-50 (gethash "10-to-50" bank-buddy-txn-size-dist 0))
         (to-100 (gethash "50-to-100" bank-buddy-txn-size-dist 0))
         (over-100 (gethash "over-100" bank-buddy-txn-size-dist 0)))
    
    (insert "| Under £10 | ")
    (insert (format "%d |\n" under-10))
    (insert "| £10 to £50 | ")
    (insert (format "%d |\n" to-50))
    (insert "| £50 to £100 | ")
    (insert (format "%d |\n" to-100))
    (insert "| Over £100 | ")
    (insert (format "%d |\n" over-100)))
  
  (insert "\n\n")
  
  ;; Subscription trend with time-based x-axis
  (insert "** Monthly Subscription Costs\n\n")
  (insert "This plot shows your estimated total subscription costs over time based on detected recurring payments.\n\n")
  
  ;; Use timecolumn format for time-based plotting
  (insert "#+PLOT: title:\"Monthly Subscription Costs\" ind:1 deps:(2) type:2d with:linespoints \
set:\"grid\" set:\"ylabel 'Cost (£)'\" set:\"xdata time\" set:\"timefmt '%Y-%m'\" \
set:\"format x '%b\\n%Y'\" set:\"xtics rotate by -45\"\n")
  
  (insert "| Month | Cost |\n")
  (insert "|-------+------|\n")
  
  ;; Create a simple estimation of subscription costs over time
  (let* ((subscriptions '())
         (monthly-by-date (make-hash-table :test 'equal)))
    
    ;; Extract subscription data
    (maphash
     (lambda (key occurrences)
       (when (>= (length occurrences) bank-buddy-subscription-min-occurrences)
         (let* ((parts (split-string key "-"))
                (sub-name (nth 0 parts))
                (amount (string-to-number (car (last parts))))
                (freq (bank-buddy-analyze-subscription-frequency occurrences)))
           
           ;; Add each occurrence to its month
           (dolist (occurrence occurrences)
             (let* ((date (car occurrence))
                    (month (substring date 0 7))
                    (monthly-cost
                     (cond
                      ((string= freq "monthly") amount)
                      ((string= freq "bi-weekly") (/ (* amount 2) 1.0))
                      ((string= freq "weekly") (/ (* amount 4) 1.0))
                      ((string= freq "annual") (/ amount 12.0))
                      (t amount))))
               
               ;; Add to monthly totals
               (puthash month
                        (+ (gethash month monthly-by-date 0) monthly-cost)
                        monthly-by-date))))))
     bank-buddy-subs)
    
    ;; Generate table rows for subscription costs by month
    (let ((month-list '()))
      ;; Convert hash to list for sorting
      (maphash (lambda (month amount) 
                 (push (cons month amount) month-list))
               monthly-by-date)
      
      ;; Sort by month chronologically
      (setq month-list (sort month-list (lambda (a b) (string< (car a) (car b)))))
      
      ;; Generate table rows
      (dolist (month-data month-list)
        (insert (format "| %s | %.2f |\n" 
                       (car month-data) (cdr month-data)))))))

;;;###autoload
(defun bank-buddy-generate-report (csv-file output-file)
  "Generate financial report from CSV-FILE and save to OUTPUT-FILE."
  (interactive "fInput CSV file: \nFOutput file: ")
  
  ;; Parse and process the data
  (bank-buddy-parse-csv-file csv-file)
  (bank-buddy-process-payments)
  
  ;; Generate report
  (with-temp-buffer
    (insert "#+title: Financial Report\n")
    (insert "#+options: toc:2 num:nil\n\n")
    
    ;; Generate report sections
    (bank-buddy-generate-summary-overview)
    (bank-buddy-generate-transaction-size-distribution)
    (bank-buddy-generate-top-spending-categories)
    (bank-buddy-generate-top-merchants)
    (bank-buddy-generate-monthly-spending)
    (bank-buddy-generate-subscriptions)
    (bank-buddy-generate-plots)  ;; Add this line to include plots
    
    ;; Write to file
    (write-file output-file))
  
  (message "Financial report generated: %s" output-file)
  
  ;; Open the report file
  (find-file output-file))

;; Utility function for development/debugging
(defun bank-buddy-debug-info ()
  "Display debug information about current data."
  (interactive)
  (let ((total-categories 0)
        (total-merchants 0)
        (total-months 0)
        (total-subs 0))
    
    (maphash (lambda (k v) (setq total-categories (1+ total-categories)))
             bank-buddy-cat-tot)
    
    (maphash (lambda (k v) (setq total-merchants (1+ total-merchants)))
             bank-buddy-merchants)
    
    (maphash (lambda (k v) (setq total-months (1+ total-months)))
             bank-buddy-monthly-totals)
    
    (maphash (lambda (k v) (setq total-subs (1+ total-subs)))
             bank-buddy-subs)
    
    (message "Processed %d categories, %d merchants, %d months, %d subscriptions"
             total-categories total-merchants total-months total-subs)))

(provide 'bank-buddy)

;;; bank-buddy.el ends here
