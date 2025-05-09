;;; bank-buddy.el --- Financial analysis and reporting -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 James Dyer
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "26.1") (async "1.9.4"))
;; Keywords: matching
;; URL: https://github.com/captainflasmr/bank-buddy
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package provides financial analysis and reporting capabilities
;; for bank statement data, it includes:
;;
;; - Transaction summaries and overviews
;; - Spending category analysis
;; - Top merchant identification
;; - Monthly spending patterns
;; - Recurring subscription detection
;;
;; The package reads CSV bank statement data *asynchronously*, categorizes
;; transactions, and generates detailed reports in Org-mode format.
;;
;;; Quick Start
;;
;;  (use-package bank-buddy)
;;  (with-eval-after-load 'bank-buddy
;;    (add-hook 'org-mode-hook 'bank-buddy-cat-maybe-enable))
;;
;;  1. Export your bank statement as a CSV file
;;  2. Edit CSV using csv-mode for all lines to DATE,DESCRIPTION,AMOUNT
;;  3. Open CSV file
;;  4. Run: =M-x bank-buddy-generate=
;;  5. Open the generated report
;;
;;; Code:

(require 'bank-buddy-cat-mode)
(require 'bank-buddy-core)
(require 'cl-lib)
(require 'async)
(require 'dired)

(defgroup bank-buddy nil
  "Customization options for bank-buddy."
  :group 'applications)

(defvar bank-buddy-monthly-transaction-counts (make-hash-table :test 'equal)
  "Hash table storing monthly transaction counts.  Populated by async callback.")

(defvar bank-buddy-unmatched-transactions-local '()
  "List of transactions that matched only the catch-all pattern.")

(defvar bank-buddy-unmatched-transactions '()
  "List of transactions that matched only the catch-all pattern.")

(defvar bank-buddy-highest-month-amount 0
  "The highest month amount.")

(defvar bank-buddy-payments '()
  "List of parsed payment transactions.  Populated by async callback.")

(defvar bank-buddy-daily-cumulative-totals (make-hash-table :test 'equal)
  "Hash table storing category totals.  Populated by async callback.")

(defvar bank-buddy-cat-tot (make-hash-table :test 'equal)
  "Hash table storing category totals.  Populated by async callback.")

(defvar bank-buddy-merchants (make-hash-table :test 'equal)
  "Hash table storing merchant totals.  Populated by async callback.")

(defvar bank-buddy-monthly-totals (make-hash-table :test 'equal)
  "Hash table storing monthly spending totals.  Populated by async callback.")

(defvar bank-buddy-txn-size-dist (make-hash-table :test 'equal)
  "Hash table for tracking transaction size distribution.")

(defvar bank-buddy-subs (make-hash-table :test 'equal)
  "Hash table for tracking potential subscriptions.  Populated by async callback.")

(defvar bank-buddy-date-first nil
  "First transaction date.  Populated by async callback.")

(defvar bank-buddy-date-last nil
  "Last transaction date.  Populated by async callback.")

(defun bank-buddy--csv-parse-buffer (first-line-contains-keys &optional buffer)
  "Parse a buffer containing CSV data, return data as a list of alists or lists.
The first line in the buffer is interpreted as a header line
if FIRST-LINE-CONTAINS-KEYS is non-nil, resulting in a list of alists.
Otherwise, return a list of lists.

If BUFFER is non-nil it gives the buffer to be parsed.  If it is
nil the current buffer is parsed."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((lines (bank-buddy--csv-parse-lines))
            header result)
        (when lines
          (if first-line-contains-keys
              (progn
                (setq header (car lines)
                      lines (cdr lines))
                (dolist (line lines)
                  (when line
                    (push (bank-buddy--csv-combine-with-header header line) result))))
            (setq result (reverse lines))))
        result))))

(defun bank-buddy--csv-parse-lines ()
  "Parse CSV lines in current buffer, returning a list of parsed lines.
Each line is represented as a list of field values."
  (let ((lines nil)
        (begin-pos (point))
        (in-quoted nil)
        (current-line nil)
        (current-field "")
        (previous-char nil))
    (while (not (eobp))
      (let ((char (char-after)))
        (cond
         ;; Handle quoted field
         ((and (eq char ?\") (not (and in-quoted (eq previous-char ?\"))))
          (if in-quoted
              (setq in-quoted nil)
            (setq in-quoted t)))
         
         ;; Handle escaped quote within quoted field
         ((and (eq char ?\") in-quoted (eq previous-char ?\"))
          (setq current-field (concat current-field "\""))
          (setq previous-char nil) ;; Reset to avoid triple quote issue
          (forward-char))
         
         ;; Handle field separator (comma)
         ((and (eq char ?,) (not in-quoted))
          (push current-field current-line)
          (setq current-field "")
          (setq begin-pos (1+ (point))))
         
         ;; Handle end of line
         ((and (eq char ?\n) (not in-quoted))
          (push current-field current-line)
          (push (reverse current-line) lines)
          (setq current-field "")
          (setq current-line nil)
          (setq begin-pos (1+ (point))))
         
         ;; Handle carriage return (part of CRLF)
         ((and (eq char ?\r) (not in-quoted))
          ;; Just skip it, we'll handle the newline next
          nil)
         
         ;; Accumulate characters for the current field
         (t
          (when (> (point) begin-pos)
            (setq current-field (concat current-field (buffer-substring-no-properties begin-pos (point)))))
          (setq current-field (concat current-field (char-to-string char)))
          (setq begin-pos (1+ (point)))))
        
        (setq previous-char char)
        (forward-char)))
    
    ;; Handle any remaining content
    (when (and (not (string-empty-p current-field)) (not current-line))
      (push current-field current-line)
      (when current-line
        (push (reverse current-line) lines)))
    
    (reverse lines)))

(defun bank-buddy--csv-combine-with-header (header line)
  "Combine HEADER and LINE into an alist."
  (let ((result nil))
    (dotimes (i (min (length header) (length line)))
      (push (cons (nth i header) (nth i line)) result))
    (reverse result)))

(defun bank-buddy-ensure-directory (dir)
  "Ensure DIR exists, creating it if necessary."
  (unless (file-exists-p dir)
    (make-directory dir t))
  dir)

(defun bank-buddy-generate-monthly-category-breakdowns (output-dir)
  "Generate separate gnuplot images for each month's category breakdown.
Images are saved in OUTPUT-DIR with filenames ordered by month.
Categories are ordered consistently based on global top spending categories."
  (let ((all-monthly-cat-data '())
        (month-count 0)
        (global-category-order (bank-buddy-get-global-category-order))
        ;; Limit to top N categories as specified in customization
        (top-categories nil))
    ;; Get top categories list limited by user setting
    (setq top-categories
          (cl-subseq global-category-order
                     0
                     (min (length global-category-order)
                          bank-buddy-core-top-spending-categories)))
    
    ;; Get all months and their category data
    (maphash
     (lambda (key value)
       (when (string-match "^\\([0-9]\\{4\\}-[0-9]\\{2\\}\\)-\\([^-]+\\)$" key)
         (let ((month (match-string 1 key))
               (category (match-string 2 key))
               (amount value))
           ;; Find or create entry for this month
           (let ((month-entry (assoc month all-monthly-cat-data)))
             (if month-entry
                 ;; Add to existing month
                 (setcdr month-entry
                         (cons (cons category amount) (cdr month-entry)))
               ;; Create new month entry
               (push (cons month (list (cons category amount)))
                     all-monthly-cat-data))))))
     bank-buddy-cat-tot)
    
    ;; Sort by month
    (setq all-monthly-cat-data
          (sort all-monthly-cat-data (lambda (a b) (string< (car b) (car a)))))
    
    (setq month-count (length all-monthly-cat-data))
    
    (insert "** Monthly Category Breakdowns\n\n")
    (insert (format "Generated %d monthly breakdown files in: %s\n\n"
                    month-count output-dir))
    (insert "#+ATTR_ORG: :width 600\n")
    ;; Process each month
    (make-directory (concat output-dir "/bank-buddy-monthly-plots"))

    (dolist (month-data all-monthly-cat-data)
      (let* ((month (car month-data))
             (cat-data-hash (make-hash-table :test 'equal)) ;; Hash for quick lookup
             (ordered-cat-data '()) ;; Will hold ordered data
             (month-total (gethash month bank-buddy-monthly-totals 0))
             (month-safe (replace-regexp-in-string "-" "" month))
             (data-file (expand-file-name (format "data-breakdown-%s.txt" month-safe) (concat output-dir "/bank-buddy-monthly-plots")))
             (image-file (expand-file-name (format "plot-%s-breakdown.png" month-safe) (concat output-dir "/bank-buddy-monthly-plots")))
             (plot-file (expand-file-name (format "plot-%s-breakdown.gp" month-safe) (concat output-dir "/bank-buddy-monthly-plots"))))
        
        (when (> month-total 0)
          ;; Put category data in hash for easy lookup
          (dolist (cat-pair (cdr month-data))
            (puthash (car cat-pair) (cdr cat-pair) cat-data-hash))
          
          ;; Create ordered list based on global category order (top categories)
          (dolist (cat-code top-categories)
            (let ((amount (gethash cat-code cat-data-hash 0)))
              (push (cons cat-code amount) ordered-cat-data)))
          
          ;; Reverse to maintain correct order
          (setq ordered-cat-data (nreverse ordered-cat-data))
          
          ;; Create data file with consistent category order
          (with-temp-file data-file
            (insert "# Category Amount Percentage\n")
            (dolist (cat ordered-cat-data)
              (let* ((cat-code (car cat))
                     (cat-name (or (cdr (assoc cat-code bank-buddy-core-category-names)) cat-code))
                     (amount (cdr cat))
                     (percentage (if (> month-total 0)
                                     (* 100.0 (/ amount month-total))
                                   0)))
                (insert (format "\"%s (%s)\" %.2f %.1f\n"
                                cat-name cat-code amount percentage)))))
          
          ;; Create gnuplot script
          (with-temp-file plot-file
            (insert (format "set terminal png size 800,600 enhanced font 'Verdana,10'\n"))
            (insert (format "set output '%s'\n"
                            (replace-regexp-in-string "\\\\" "\\\\\\\\" image-file)))
            (insert "set style data histogram\n")
            (insert "set style fill solid 0.8 border -1\n")
            (insert "set xtics rotate by -45\n")
            (insert "set key off\n")
            (insert (format "set title 'Category Breakdown for %s (Total: £%.2f)'\n"
                            month month-total))
            (insert "set ylabel 'Amount (£)'\n")
            (insert "set yrange [0:*]\n")
            (insert "set grid y\n")
            (insert "set boxwidth 0.8 relative\n")
            (insert (format "plot '%s' using 2:xtic(1) with boxes lc rgb '#4169E1'\n"
                            (replace-regexp-in-string "\\\\" "\\\\\\\\" data-file))))
          
          ;; Run gnuplot
          (call-process "gnuplot" nil nil nil plot-file)
          
          ;; Insert entry in report
          (insert (format "[[file:%s]]\n"
                          (file-relative-name image-file output-dir))))))
    
    ;; Add instructions for viewing
    (insert "*** Viewing Monthly Breakdowns Sequentially\n\n")
    (insert "To view the monthly breakdowns in sequence:\n\n")
    (insert "1. Open an image viewer that supports wildcard patterns\n")
    (insert (format "2. Navigate to: %s\n" output-dir))
    (insert "3. Open the pattern: plot-*-breakdown.png\n\n")
    (insert "Many image viewers will allow you to step through these images in chronological order.\n\n")
    (insert "Note: Categories in all plots are ordered consistently based on the top-spending categories ")
    (insert (format "across the entire time period (limited to top %d categories).\n" bank-buddy-core-top-spending-categories))))

(defun bank-buddy-generate-monthly-categories-table (output-dir)
  "Generate a comprehensive category table with external gnuplot using OUTPUT-DIR."
  (let* ((all-months (sort (hash-table-keys bank-buddy-monthly-totals) #'string>))
         (global-category-order (bank-buddy-get-global-category-order))
         (top-categories (cl-subseq global-category-order
                                    0
                                    (min (length global-category-order)
                                         bank-buddy-core-top-spending-categories)))
         (totals-by-category (make-hash-table :test 'equal)))

    (insert "\n** Monthly Spending by Category\n\n")
    (insert "This table shows spending breakdown for each month by top categories:\n\n")
    
    ;; Create the table header with categories as columns
    (insert "#+NAME: monthly-categories-table\n")
    (insert "| Month | Total ")
    
    ;; Add each category as a column
    (dolist (cat top-categories)
      (insert (format "| %s " cat)))
    
    (insert "|\n")
    
    ;; Add rows for each month
    (dolist (month all-months)
      (let ((monthly-total (gethash month bank-buddy-monthly-totals 0)))
        (insert (format "| %s | %.2f " month monthly-total))
        
        (dolist (cat top-categories)
          (let* ((month-cat-key (concat month "-" cat))
                 (cat-amount (gethash month-cat-key bank-buddy-cat-tot 0)))
            (puthash cat
                     (+ (gethash cat totals-by-category 0) cat-amount)
                     totals-by-category)
            (insert (format "| %.2f " cat-amount))))
        
        (insert "|\n")))
    
    (insert "\n*** Monthly Spending Visualization (Stacked Categories)\n\n")
    (insert "The following visualization shows monthly spending with each bar stacked by category:\n\n")

    ;; Generate data file for stacked histogram
    (let* ((data-file (expand-file-name "monthly-categories-stacked.dat" output-dir))
           (plot-file (expand-file-name "monthly-categories-stacked.gp" output-dir))
           (image-file (expand-file-name "financial-report--monthly-spending-stacked.png" output-dir)))
      
      ;; Create data file (months in reverse chronological order)
      (with-temp-file data-file
        (insert "Month Total")
        (dolist (cat top-categories)
          (insert (format " %s" cat)))
        (insert "\n")
        
        (dolist (month (reverse all-months))
          (let ((monthly-total (gethash month bank-buddy-monthly-totals 0)))
            (insert (format "%s %.2f" month monthly-total))
            
            (dolist (cat top-categories)
              (let* ((month-cat-key (concat month "-" cat))
                     (cat-amount (gethash month-cat-key bank-buddy-cat-tot 0)))
                (insert (format " %.2f" cat-amount))))
            
            (insert "\n"))))
      
      ;; Create gnuplot script for stacked histogram
      (with-temp-file plot-file
        (insert (format "set terminal png size 1200,600 enhanced font 'Verdana,10'\n"))
        (insert (format "set output '%s'\n" image-file))
        (insert "set style data histograms\n")
        (insert "set style histogram rowstacked\n")
        (insert "set boxwidth 0.75 relative\n")
        (insert "set style fill solid 1.0 border -1\n")
        (insert "set title 'Monthly Spending by Category'\n")
        (insert "set xlabel 'Month'\n")
        (insert "set ylabel 'Amount (£)'\n")
        (insert "set xtics rotate by -45\n")
        (insert "set key outside right top vertical\n")
        (insert "set auto x\n")
        (insert "set yrange [0:*]\n")
        (insert "set grid ytics\n")
        (insert (format "plot for [i=3:%d] '%s' using i:xtic(1) title columnheader(i), \\\n"
                        (+ 3 (length top-categories) -1) data-file))
        (insert (format "     '%s' using ($0-1):2 with linespoints \\\n" data-file))
        (insert "     linecolor rgb \"#000000\" linewidth 3 pointtype 7 pointsize 1.5 title \"Total\"\n"))
      
      ;; Execute gnuplot
      (call-process "gnuplot" nil nil nil plot-file)
      
      ;; Insert into org buffer
      (insert "#+ATTR_ORG: :width 800\n")
      (insert (format "[[file:%s]]\n\n" (file-relative-name image-file output-dir)))
      (insert (format "Generated from gnuplot script: [[file:%s]]\n" (file-relative-name plot-file output-dir)))
      (insert (format "Data file: [[file:%s]]\n\n" (file-relative-name data-file output-dir))))
    
    ;; Second visualization - Individual Categories
    (insert "\n*** Monthly Spending with Individual Categories\n\n")
    (insert "This plot shows each category separately across months for detailed comparison:\n\n")
    
    (let* ((data-file (expand-file-name "monthly-categories-lines.dat" output-dir))
           (plot-file (expand-file-name "monthly-categories-lines.gp" output-dir))
           (image-file (expand-file-name "financial-report--monthly-spending-categories.png" output-dir)))
      
      ;; Create data file for line plot (same as above)
      (with-temp-file data-file
        (insert "Month Total")
        (dolist (cat top-categories)
          (insert (format " %s" cat)))
        (insert "\n")
        
        (dolist (month (reverse all-months))
          (let ((monthly-total (gethash month bank-buddy-monthly-totals 0)))
            (insert (format "%s %.2f" month monthly-total))
            
            (dolist (cat top-categories)
              (let* ((month-cat-key (concat month "-" cat))
                     (cat-amount (gethash month-cat-key bank-buddy-cat-tot 0)))
                (insert (format " %.2f" cat-amount))))
            
            (insert "\n"))))
      
      ;; Create gnuplot script for line plot
      (with-temp-file plot-file
        (insert (format "set terminal png size 1200,600 enhanced font 'Verdana,10'\n"))
        (insert (format "set output '%s'\n" image-file))
        (insert "set title 'Monthly Spending by Category'\n")
        (insert "set xlabel 'Month'\n")
        (insert "set ylabel 'Amount (£)'\n")
        (insert "set style data linespoints\n")
        (insert "set key outside right top vertical\n")
        (insert "set xtics rotate by -45\n")
        (insert "set grid\n")
        (insert "set auto x\n")
        (insert (format "plot for [i=3:%d] '%s' using 0:i:xtic(1) title columnheader(i) with linespoints pointtype i-2 lw 2\n"
                        (+ 3 (length top-categories) -1) data-file)))
      
      ;; Execute gnuplot
      (call-process "gnuplot" nil nil nil plot-file)
      
      ;; Insert into org buffer
      (insert "#+ATTR_ORG: :width 800\n")
      (insert (format "[[file:%s]]\n\n" (file-relative-name image-file output-dir)))
      (insert (format "Generated from gnuplot script: [[file:%s]]\n" (file-relative-name plot-file output-dir)))
      (insert (format "Data file: [[file:%s]]\n" (file-relative-name data-file output-dir))))))

(defun bank-buddy-get-month-category-totals (month)
  "Get the totals for each category in the specified MONTH."
  (let ((cat-totals '()))
    (maphash (lambda (key value)
               (when (string-prefix-p month key)
                 (let* ((parts (split-string key "-"))
                        (category (when (>= (length parts) 2)
                                    (nth (1- (length parts)) parts))))
                   (when category
                     (push (cons category value) cat-totals)))))
             bank-buddy-cat-tot)
    cat-totals))

(defun bank-buddy-get-global-category-order ()
  "Determine the global category order based on total spending across all months."
  (let ((cat-totals (make-hash-table :test 'equal)))
    ;; Aggregate spending across all months by category
    (maphash (lambda (key value)
               (let* ((parts (split-string key "-"))
                      (category (when (>= (length parts) 2)
                                  (nth (1- (length parts)) parts))))
                 (when category
                   (puthash category
                            (+ (gethash category cat-totals 0) value)
                            cat-totals))))
             bank-buddy-cat-tot)
    
    ;; Convert to list and sort by total amount (descending)
    (let ((cat-list '()))
      (maphash (lambda (category amount)
                 (push (cons category amount) cat-list))
               cat-totals)
      (setq cat-list (sort cat-list (lambda (a b) (> (cdr a) (cdr b)))))
      
      ;; Return just the ordered category codes
      (mapcar #'car cat-list))))

(defun bank-buddy-generate-category-bar (month global-category-order bar-width)
  "Generate a text-based bar showing category spending for MONTH.
GLOBAL-CATEGORY-ORDER is the ordered list of categories.
BAR-WIDTH is the maximum width of the bar in characters."
  (let* ((month-total bank-buddy-highest-month-amount)
         (cat-totals-hash (make-hash-table :test 'equal))
         (bar-text "")
         (num-categories-shown 0)
         (max-categories-to-show bank-buddy-core-monthly-spending-max-bar-categories)) ;; Limit number of categories to keep visual clean
    
    ;; Convert month's category totals to hash for easy lookup
    (dolist (cat-pair (bank-buddy-get-month-category-totals month))
      (puthash (car cat-pair) (cdr cat-pair) cat-totals-hash))
    
    ;; Now build the bar with consistent category order
    (dolist (category global-category-order)
      (let ((amount (gethash category cat-totals-hash 0)))
        (when (and (> amount 0)
                   (< num-categories-shown max-categories-to-show))
          (let* ((proportion (/ amount month-total))
                 (segment-width (round (* proportion bar-width)))
                 (display-text (format "%s%.0f" category (/ (float amount) 100)))
                 (padding-length (- segment-width (length display-text) 1)))

            (when (>= padding-length 0)
              (setq bar-text (concat bar-text
                                     display-text
                                     (make-string padding-length ?_)
                                     "/"))
              (setq num-categories-shown (1+ num-categories-shown)))))))
    
    ;; Return the complete bar
    (if (string= bar-text "")
        (make-string 3 ?-) ;; Return minimal bar if no data
      (concat "/" bar-text))))

(defun bank-buddy-generate-unmatched-transactions ()
  "Generate transactions that weren't matched by specific patterns."
  (insert "\n* Unmatched Transactions\n\n")
  (insert "The following transactions were only matched by the catch-all pattern (\".*\"). ")
  (insert "You may want to add specific patterns for these in `bank-buddy-core-cat-list-defines`\n\n")
  
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
        (insert (format "[%s] %s" (format-time-string "%T") message))
        (unless (looking-at "$") (insert "\n"))))
    ;; Display buffer if not already visible
    (unless (get-buffer-window buf)
      (display-buffer buf))))

(defun bank-buddy-parse-date (date-string)
  "Parse DATE-STRING into a time value, working across Emacs versions."
  (condition-case nil
      (if (version< emacs-version "29.1")
          ;; For Emacs versions before 29.1
          (let* ((parsed (parse-time-string date-string))
                 (year (nth 5 parsed))
                 (month (nth 4 parsed))
                 (day (nth 3 parsed))
                 (current-time (decode-time)))
            (encode-time 0 0 0 day month year (nth 8 current-time)))
        ;; For Emacs 29.1 and later
        (date-to-time date-string))
    (error nil)))

(defun bank-buddy-days-between (date1 date2)
  "Calculate days between DATE1 and DATE2 in YYYY-MM-DD format."
  (let ((time1 (bank-buddy-parse-date date1))
        (time2 (bank-buddy-parse-date date2)))
    (if (and time1 time2)
        (floor (/ (float-time (time-subtract time2 time1)) 86400))
      0))) ; Return 0 if either date is invalid

;; Helper function for date calculation (no change needed)
;; (defun bank-buddy-days-between (date1 date2)
;;   "Calculate days between DATE1 and DATE2 in YYYY-MM-DD format."
;;   (let ((time1 (date-to-time date1))
;;         (time2 (date-to-time date2)))
;;     (floor (/ (float-time (time-subtract time2 time1)) 86400))))

(defun bank-buddy--categorize-payment-local (name debit month date cat-tot merchants monthly-totals txn-size-dist subs)
  "Categorize payment based on NAME, DEBIT amount, MONTH and DATE.
Argument CAT-TOT category total.
Argument MERCHANTS list of merchants.
Argument MONTHLY-TOTALS monthly totals.
Argument TXN-SIZE-DIST .
Argument SUBS ."
  (let ((category-found nil)
        (split-key nil)
        (merchant (replace-regexp-in-string " .*" "" name))
        (unmatched t)) ; New flag to track if transaction matched only catch-all

    ;; Find category (uses global bank-buddy-core-cat-list-defines read by the child process)
    (cl-loop for category in bank-buddy-core-cat-list-defines
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

    ;; Track potential subscriptions (local hash, uses global bank-buddy-core-subscription-patterns)
    (cl-loop for (pattern . sub-name) in bank-buddy-core-subscription-patterns
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
  "Parse CSV-FILE and process payments.  Return processed data as a list.
This function runs in a separate process via async.el."
  ;; Ensure required libraries are loaded in the child process
  (require 'cl-lib)
  ;; It also implicitly needs bank-buddy loaded for helper functions and variables.
  ;; Assuming bank-buddy is 'required' or 'loaded' via the async-start call.

  ;; --- Use local variables, DO NOT modify globals directly ---
  (let ((payments '())
        (cat-tot (make-hash-table :test 'equal))
        (merchants (make-hash-table :test 'equal))
        (monthly-totals (make-hash-table :test 'equal))
        (daily-cumulative-totals (make-hash-table :test 'equal))
        (txn-size-dist (make-hash-table :test 'equal))
        (subs (make-hash-table :test 'equal))
        (bank-buddy-unmatched-transactions-local '())
        (monthly-transaction-counts (make-hash-table :test 'equal))
        (date-first nil)
        (date-last nil)
        (error-occurred nil)
        (last-processed-date nil))

    ;; --- 1. Parsing (similar to bank-buddy-parse-csv-file) ---
    (condition-case err
        (with-temp-buffer
          (insert-file-contents csv-file)
          (setq payments (bank-buddy--csv-parse-buffer t)))
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
                    (total-amount 0)
                    (previous-day-total 0)
                    (current-month ""))
                (dolist (payment payments)
                  (let* ((date-cell (nth 0 payment))
                         (desc-cell (nth 1 payment))
                         (debit-cell (nth 2 payment))
                         (date (if date-cell (cdr date-cell) "")) ; Handle potentially nil cells
                         (month (if (>= (length date) 7) (substring date 0 7) "UNKNOWN"))
                         (day (if (>= (length date) 10) (substring date 8 10) "00"))
                         (description (if desc-cell (cdr desc-cell) ""))
                         (debit-str (if debit-cell (cdr debit-cell) ""))
                         (debit (if (string-blank-p debit-str) 0 (string-to-number debit-str))))

                    ;; Skip large transactions if configured
                    (when (and (numberp debit) ; Ensure debit is a number
                               (or (not bank-buddy-core-exclude-large-txns)
                                   (< debit bank-buddy-core-large-txn-threshold)))

                      ;; Fill in missing days up to the end of the month
                      (when last-processed-date
                        (let* ((last-month (substring last-processed-date 0 7))
                               (last-day (string-to-number (substring last-processed-date 8 10))))
                          (dotimes (i (- 31 last-day))
                            (let* ((fill-day (format "%02d" (+ last-day i 1)))
                                   (fill-key (concat last-month "-" fill-day)))
                              (puthash fill-key previous-day-total daily-cumulative-totals)))))
                      
                      ;; Reset previous-day-total if we are in a new month
                      (when (not (string= current-month month))
                        (setq current-month month)
                        (setq previous-day-total 0))
                      
                      (let ((month-day-key (concat month "-" day)))
                        ;; Calculate the cumulative total for the current day
                        (setq previous-day-total (+ previous-day-total debit))
                        ;; Store the new cumulative total for the current day
                        (puthash month-day-key previous-day-total daily-cumulative-totals))

                      (setq last-processed-date date) ; Update the last processed date

                      (when (> debit 0) ;; Only count positive debits
                        ;; Increment monthly transaction count
                        (puthash month
                                 (1+ (gethash month monthly-transaction-counts 0))
                                 monthly-transaction-counts)
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
          :daily-cumulative-totals daily-cumulative-totals
          :txn-size-dist txn-size-dist
          :subs subs
          :unmatched-transactions bank-buddy-unmatched-transactions-local
          :monthly-transaction-counts monthly-transaction-counts)))

;; These functions assume the global variables have been populated by the async callback.

(defun bank-buddy-generate-monthly-progress-comparison (output-dir)
  "Generate a plot comparing monthly spending progress to OUTPUT-DIR."
  (let* ((months (sort (hash-table-keys bank-buddy-monthly-totals) #'string<))
         (data-file (expand-file-name "monthly-progress-comparison.dat" output-dir))
         (plot-file (expand-file-name "monthly-progress-comparison.gp" output-dir))
         (image-file (expand-file-name "monthly-progress-comparison.png" output-dir)))
    
    ;; Generate data file
    (with-temp-file data-file
      (insert "Day")
      (dolist (month months)
        (insert (format "\t%s" month)))
      (insert "\n")
      
      (dotimes (day 31)
        (let ((day-str (format "%02d" (1+ day))))
          (insert day-str)
          (dolist (month months)
            (let* ((month-day-key (concat month "-" day-str))
                   (amount (gethash month-day-key bank-buddy-daily-cumulative-totals 0)))
              (insert (format "\t%.2f" amount))))
          (insert "\n"))))
    
    ;; Generate gnuplot script with dark background and improved visibility
    (with-temp-file plot-file
      (insert "set terminal png size 800,600 enhanced font 'Verdana,10'\n")
      (insert (format "set output '%s'\n" image-file))
      
      ;; Set dark background and light text/border colors
      (insert "set object 1 rectangle from screen 0,0 to screen 1,1 fillcolor rgb \"#000000\" behind\n")
      (insert "set border lc rgb \"#FFFFFF\"\n")
      (insert "set title 'Monthly Spending Progress Comparison' textcolor rgb \"#FFFFFF\"\n")
      (insert "set xlabel 'Day of Month' textcolor rgb \"#FFFFFF\"\n")
      (insert "set ylabel 'Cumulative Spending (£)' textcolor rgb \"#FFFFFF\"\n")
      (insert "set key outside right textcolor rgb \"#FFFFFF\"\n")
      (insert "set xtics 1,5 textcolor rgb \"#FFFFFF\"\n")
      (insert "set ytics textcolor rgb \"#FFFFFF\"\n")
      (insert "set grid lc rgb \"#555555\"\n")
      
      ;; Define bright, high-contrast line colors with increased thickness
      (insert "set linetype 1 lc rgb \"#FF0000\" lw 1\n")  ;; Red
      (insert "set linetype 2 lc rgb \"#00FFFF\" lw 1\n")  ;; Cyan
      (insert "set linetype 3 lc rgb \"#00FF00\" lw 1\n")  ;; Bright green
      (insert "set linetype 4 lc rgb \"#FF00FF\" lw 1\n")  ;; Magenta
      (insert "set linetype 5 lc rgb \"#FFFF00\" lw 1\n")  ;; Yellow
      
      ;; Add more line types if needed for additional months
      (when (> (length months) 5)
        (insert "set linetype 6 lc rgb \"#FF8000\" lw 1\n")  ;; Orange
        (insert "set linetype 7 lc rgb \"#8080FF\" lw 1\n")  ;; Light blue
        (insert "set linetype 8 lc rgb \"#00FF80\" lw 1\n")  ;; Mint green
        (insert "set linetype 9 lc rgb \"#FF80FF\" lw 1\n")  ;; Light pink
        (insert "set linetype 10 lc rgb \"#FFFFFF\" lw 1\n")) ;; White
      
      ;; Plot with explicit line types
      (insert (format "plot for [i=2:%d] '%s' using 1:i with lines lt (i-1) title columnhead\n"
                      (1+ (length months)) data-file)))
    
    ;; Run gnuplot
    (call-process "gnuplot" nil nil nil plot-file)
    
    ;; Insert the plot into the report
    (insert "\n** Monthly Spending Progress Comparison\n\n")
    (insert "This plot compares the cumulative spending progress for each month:\n\n")
    (insert "#+ATTR_ORG: :width 800\n")
    (insert (format "[[file:%s]]\n\n" (file-relative-name image-file output-dir)))
    (insert "The plot shows how spending in the current month compares to previous months at the same point in time.\n")
    (insert (format "\nGenerated from gnuplot script: [[file:%s]]\n" (file-relative-name plot-file output-dir)))
    (insert (format "Data file: [[file:%s]]\n" (file-relative-name data-file output-dir)))))

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
    (maphash (lambda (_k v)
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
          (insert (format "- *Average Weekly Spending:* £%.2f\n"
                          (* (/ total-spending date-range-days) 7))))
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

(defun bank-buddy-generate-top-spending-categories (output-dir)
  "Generate top spending categories section with external gnuplot using OUTPUT-DIR."
  (let ((categories '())
        (total-spending 0)
        (total-months 0)
        (first-month nil)
        (last-month nil))

    ;; Sum up spending by category
    (maphash (lambda (key value)
               (let* ((parts (split-string key "-"))
                      (category (if (> (length parts) 2) (nth (1- (length parts)) parts) "unknown"))
                      (existing (assoc category categories)))
                 (if existing
                     (setcdr existing (+ (cdr existing) value))
                   (push (cons category value) categories))
                 
                 (when (>= (length parts) 2)
                   (let ((month-year (concat (nth 0 parts) "-" (nth 1 parts))))
                     (when (or (not first-month) (string< month-year first-month))
                       (setq first-month month-year))
                     (when (or (not last-month) (string> month-year last-month))
                       (setq last-month month-year))))))
             bank-buddy-cat-tot)

    ;; Calculate total spending
    (setq total-spending (apply #'+ (mapcar #'cdr categories)))
    
    ;; Calculate number of months from the monthly-totals hash
    (setq total-months (hash-table-count bank-buddy-monthly-totals))
    
    ;; If no months data in the hash, try to calculate from first and last months
    (when (and (= total-months 0) first-month last-month)
      (let* ((first-parts (split-string first-month "-"))
             (last-parts (split-string last-month "-"))
             (first-year (string-to-number (nth 0 first-parts)))
             (first-month-num (string-to-number (nth 1 first-parts)))
             (last-year (string-to-number (nth 0 last-parts)))
             (last-month-num (string-to-number (nth 1 last-parts))))
        (setq total-months (+ (* 12 (- last-year first-year))
                              (- last-month-num first-month-num)
                              1))))
    
    (when (< total-months 1)
      (setq total-months 1))

    ;; Sort by amount (descending)
    (setq categories (sort categories (lambda (a b) (> (cdr a) (cdr b)))))

    (insert "\n* Top Spending Categories\n\n")
    (if (and categories (> total-spending 0))
        (progn
          (insert (format "Analysis of spending across %d months, showing category breakdown:\n\n" total-months))
          
          ;; Create the table
          (insert "#+NAME: top-spending-categories\n")
          (insert "| Category | Total Spend | Percentage | Monthly Avg | Yearly Avg |\n")
          (insert "|----------+------------+------------+-------------+------------|\n")
          
          ;; Add rows for each category
          (let ((counter 0))
            (dolist (cat categories)
              (when (< counter bank-buddy-core-top-spending-categories)
                (let* ((cat-code (car cat))
                       (cat-name (cdr (assoc cat-code bank-buddy-core-category-names)))
                       (amount (cdr cat))
                       (percentage (* 100.0 (/ amount total-spending)))
                       (monthly-avg (/ amount (float total-months)))
                       (yearly-avg (* 12 monthly-avg)))
                  (insert (format "| /%s/ %s | %11.2f | %10.1f%% | %11.2f | %10.2f |\n"
                                  cat-code
                                  (or cat-name cat-code)
                                  amount
                                  percentage
                                  monthly-avg
                                  yearly-avg))
                  (setq counter (1+ counter))))))

          ;; Generate external gnuplot file and execute
          (let* ((data-file (expand-file-name "top-spending-categories.dat" output-dir))
                 (plot-file (expand-file-name "top-spending-categories.gp" output-dir))
                 (image-file (expand-file-name "financial-report--top-spending-categories.png" output-dir)))
            
            ;; Create data file
            (with-temp-file data-file
              (insert "# Category Amount Percentage MonthlyAvg YearlyAvg\n")
              (let ((counter 0))
                (dolist (cat categories)
                  (when (< counter bank-buddy-core-top-spending-categories)
                    (let* ((cat-code (car cat))
                           (cat-name (cdr (assoc cat-code bank-buddy-core-category-names)))
                           (amount (cdr cat))
                           (percentage (* 100.0 (/ amount total-spending)))
                           (monthly-avg (/ amount (float total-months)))
                           (yearly-avg (* 12 monthly-avg)))
                      (insert (format "\"%s_%s\" %.2f %.1f %.2f %.2f\n"
                                      cat-code
                                      (or cat-name cat-code)
                                      amount
                                      percentage
                                      monthly-avg
                                      yearly-avg))
                      (setq counter (1+ counter)))))))
            
            ;; Create gnuplot script file
            (with-temp-file plot-file
              (insert (format "set terminal png size 800,600 enhanced font 'Verdana,10'\n"))
              (insert (format "set output '%s'\n" image-file))
              (insert "set style data histogram\n")
              (insert "set style fill solid\n")
              (insert "set boxwidth 0.8\n")
              (insert "set xtics rotate by -45\n")
              (insert "set ylabel \"Amount\"\n")
              (insert "set title \"Top Spending Categories\"\n")
              (insert (format "plot '%s' using 2:xtic(1) with boxes title \"Amount\"\n" data-file)))
            
            ;; Execute gnuplot
            (call-process "gnuplot" nil nil nil plot-file)
            
            ;; Insert image into org buffer
            (insert "\n#+ATTR_ORG: :width 600\n")
            (insert (format "[[file:%s]]\n\n" (file-relative-name image-file output-dir)))
            
            (insert "Monthly and yearly averages are calculated based on the total duration of the data.\n")
            (insert (format "Data spans approximately %d months.\n" total-months))
            (insert (format "\nGenerated from gnuplot script: [[file:%s]]\n" (file-relative-name plot-file output-dir)))
            (insert (format "Data file: [[file:%s]]\n" (file-relative-name data-file output-dir)))))
      (insert "No category spending data available.\n"))))

(defun bank-buddy-generate-top-merchants (output-dir)
  "Generate top merchants section with external gnuplot using OUTPUT-DIR."
  (let ((merchants-list '())
        (total-spending 0)
        (total-months 0))

    ;; Convert hash to list for sorting
    (maphash (lambda (k v)
               (push (cons k v) merchants-list)
               (setq total-spending (+ total-spending v)))
             bank-buddy-merchants)

    ;; Calculate number of months from the monthly-totals hash
    (setq total-months (hash-table-count bank-buddy-monthly-totals))
    
    ;; If no months data in the hash, try to extract from date range
    (when (and (= total-months 0) bank-buddy-date-first bank-buddy-date-last)
      (let* ((first-parts (split-string bank-buddy-date-first "-"))
             (last-parts (split-string bank-buddy-date-last "-"))
             (first-year (string-to-number (nth 0 first-parts)))
             (first-month-num (string-to-number (nth 1 first-parts)))
             (last-year (string-to-number (nth 0 last-parts)))
             (last-month-num (string-to-number (nth 1 last-parts))))
        (when (and (>= (length first-parts) 2) (>= (length last-parts) 2))
          (setq total-months (+ (* 12 (- last-year first-year))
                                (- last-month-num first-month-num)
                                1)))))
    
    ;; Fallback if we still don't have a valid month count
    (when (or (< total-months 1) (not (numberp total-months)))
      (setq total-months 1))

    ;; Sort by amount (descending)
    (setq merchants-list (sort merchants-list (lambda (a b) (> (cdr a) (cdr b)))))

    (insert "\n* Top Merchants\n\n")
    (if merchants-list
        (progn
          ;; Add a summary section with overall totals
          (insert (format "Analysis of merchant spending across %d months:\n\n" total-months))
          (insert (format "- *Total merchant spending:* £%.2f\n" total-spending))
          (insert (format "- *Monthly average (all merchants):* £%.2f\n"
                          (/ total-spending (float total-months))))
          (insert (format "- *Yearly average (all merchants):* £%.2f\n\n"
                          (* 12 (/ total-spending (float total-months)))))
          
          ;; Create the table header
          (insert "#+NAME: top-merchants\n")
          (insert "| Merchant | Total Spend | Percentage | Monthly Avg | Yearly Avg |\n")
          (insert "|---------+------------+------------+-------------+------------|\n")
          
          ;; Add rows for each merchant up to the limit
          (let ((counter 0))
            (dolist (merchant merchants-list)
              (when (< counter bank-buddy-core-top-merchants)
                (let* ((merchant-name (car merchant))
                       (amount (cdr merchant))
                       (percentage (* 100.0 (/ amount total-spending)))
                       (monthly-avg (/ amount (float total-months)))
                       (yearly-avg (* 12 monthly-avg)))
                  (insert (format "| %s | %11.2f | %10.1f%% | %11.2f | %10.2f |\n"
                                  merchant-name
                                  amount
                                  percentage
                                  monthly-avg
                                  yearly-avg))
                  (setq counter (1+ counter))))))

          ;; Create data file and gnuplot script
          (let* ((data-file (expand-file-name "top-merchants.dat" output-dir))
                 (plot-file (expand-file-name "top-merchants.gp" output-dir))
                 (image-file (expand-file-name "financial-report--top-merchants.png" output-dir)))
            
            ;; Create data file
            (with-temp-file data-file
              (insert "Merchant Amount Percentage MonthlyAvg YearlyAvg\n")
              (let ((counter 0))
                (dolist (merchant merchants-list)
                  (when (< counter bank-buddy-core-top-merchants)
                    (let* ((merchant-name (car merchant))
                           (amount (cdr merchant))
                           (percentage (* 100.0 (/ amount total-spending)))
                           (monthly-avg (/ amount (float total-months)))
                           (yearly-avg (* 12 monthly-avg)))
                      (insert (format "\"%s\" %.2f %.1f %.2f %.2f\n"
                                      merchant-name
                                      amount
                                      percentage
                                      monthly-avg
                                      yearly-avg))
                      (setq counter (1+ counter)))))))
            
            ;; Create gnuplot script
            (with-temp-file plot-file
              (insert (format "set terminal png size 800,600 enhanced font 'Verdana,10'\n"))
              (insert (format "set output '%s'\n" image-file))
              (insert "set style data histogram\n")
              (insert "set style fill solid\n")
              (insert "set boxwidth 0.8\n")
              (insert "set xtics rotate by -45\n")
              (insert "set ylabel \"Amount\"\n")
              (insert "set title \"Top Merchants\"\n")
              (insert (format "plot '%s' using 2:xtic(1) with boxes title \"Amount\"\n" data-file)))
            
            ;; Execute gnuplot
            (call-process "gnuplot" nil nil nil plot-file)
            
            ;; Insert into org buffer
            (insert "\n#+ATTR_ORG: :width 600\n")
            (insert (format "[[file:%s]]\n\n" (file-relative-name image-file output-dir)))
            
            ;; Add links to gnuplot files
            (insert "Monthly and yearly averages are calculated based on the total duration of the data.\n")
            (insert (format "Generated from gnuplot script: [[file:%s]]\n" (file-relative-name plot-file output-dir)))
            (insert (format "Data file: [[file:%s]]\n" (file-relative-name data-file output-dir)))))
      (insert "No merchant spending data available.\n"))))

(defun bank-buddy-generate-monthly-spending ()
  "Create a monthly spending report."
  (let* ((months-list '())
         (year-months (mapcar #'identity (hash-table-keys bank-buddy-monthly-totals))) ; Get keys
         (total-spending 0)
         (month-count 0)
         highest-month lowest-month
         highest-amount lowest-amount
         (global-category-order nil)
         (all-monthly-cat-data '())) ; Added to collect category data for each month

    ;; Check if year-months is not empty before sorting
    (when year-months
      (setq year-months (sort year-months #'string<))
      (setq global-category-order (bank-buddy-get-global-category-order))

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
                      lowest-amount amount)))
          
          ;; Collect category data for this month
          (let ((month-cat-totals (bank-buddy-get-month-category-totals month)))
            (push (cons month month-cat-totals) all-monthly-cat-data)))))

    (setq bank-buddy-highest-month-amount highest-amount)
    
    (insert "\n* Monthly Spending Patterns\n\n")
    (if (> month-count 0)
        (progn
          ;; Summary section
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

          ;; Text-based visualization (keep the existing one)
          (insert "** Text-Based Category Visualization\n\n")
          (insert "Each bar shows spending by category. The 3-letter codes represent categories,\n")
          (insert "with consistent ordering by overall spending (highest to lowest) across all months.\n")
          (insert "The length of each segment is proportional to its share of that month's spending.\n\n")
          (insert "#+begin_src\n")
          ;; Sort the list representation for output
          (setq months-list (sort months-list (lambda (a b) (string< (car b) (car a)))))
          
          (let ((bar-width bank-buddy-core-monthly-spending-bar-width)) ;; Wider bar to accommodate text-based visualization
            (dolist (month-data months-list)
              (let* ((month (car month-data))
                     (amount (cdr month-data))
                     (bar-text (bank-buddy-generate-category-bar
                                month global-category-order bar-width)))
                (insert (format "%s *£%4.0f* %s\n" month amount bar-text)))))
          (insert "#+end_src\n")
          
          ;; Create category breakdown tables for each month
          (setq all-monthly-cat-data (sort all-monthly-cat-data
                                           (lambda (a b) (string< (car a) (car b))))))
          (insert "No monthly spending data available.\n"))))

(defun bank-buddy-generate-monthly-transaction-counts (output-dir)
  "Generate a section showing transaction counts for each month to OUTPUT-DIR."
  (let ((months-list '())
        (total-transactions 0)
        (total-spending 0))
    
    (insert "\n* Monthly Transaction Counts\n\n")
    (insert "This section shows the number of transactions processed for each month.\n\n")
    
    ;; Create the table
    (insert "#+NAME: monthly-transactions\n")
    (insert "| Month      | Transactions | Spending (£) | Avg. per Transaction (£) |\n")
    (insert "|------------+--------------+--------------+--------------------------|\n")
    
    ;; Get months and sort
    (maphash (lambda (month count)
               (let ((amount (gethash month bank-buddy-monthly-totals 0)))
                 (push (list month count amount) months-list)))
             bank-buddy-monthly-transaction-counts)
    (setq months-list (nreverse (sort months-list (lambda (a b) (string< (car a) (car b))))))
    
    ;; Display each month
    (dolist (month-data months-list)
      (let* ((month (nth 0 month-data))
             (txn-count (nth 1 month-data))
             (amount (nth 2 month-data))
             (avg-per-txn (if (> txn-count 0) (/ amount txn-count) 0)))
        (insert (format "| %s | %12d | %12.2f | %24.2f |\n"
                        month txn-count amount avg-per-txn))
        (setq total-transactions (+ total-transactions txn-count))
        (setq total-spending (+ total-spending amount))))
    
    ;; Create data file and gnuplot script
    (let* ((data-file (expand-file-name "monthly-transactions.dat" output-dir))
           (plot-file (expand-file-name "monthly-transactions.gp" output-dir))
           (image-file (expand-file-name "financial-report--monthly-transactions.png" output-dir)))
      
      ;; Create data file
      (with-temp-file data-file
        (insert "Month Transactions Spending Average\n")
        (dolist (month-data months-list)
          (let* ((month (nth 0 month-data))
                 (txn-count (nth 1 month-data))
                 (amount (nth 2 month-data))
                 (avg-per-txn (if (> txn-count 0) (/ amount txn-count) 0)))
            (insert (format "%s %d %.2f %.2f\n" month txn-count amount avg-per-txn)))))
      
      ;; Create gnuplot script
      (with-temp-file plot-file
        (insert (format "set terminal png size 1000,600 enhanced font 'Verdana,10'\n"))
        (insert (format "set output '%s'\n" image-file))
        (insert "set title 'Monthly Transaction Counts and Spending'\n")
        (insert "set xlabel 'Month'\n")
        (insert "set ylabel 'Number of Transactions'\n")
        (insert "set y2label 'Spending (£)'\n")
        (insert "set ytics nomirror\n")
        (insert "set y2tics\n")
        (insert "set xtics rotate by -45\n")
        (insert "set key outside right top\n")
        (insert "set style data histogram\n")
        (insert "set style fill solid 1.0\n")
        (insert "set boxwidth 0.4\n")
        (insert "set offset 0,0,0,0\n")
        (insert "set y2range [0:*]\n")
        (insert "set yrange [0:*]\n")
        (insert (format "plot '%s' using 2:xtic(1) with boxes axes x1y1 title 'Transactions' lc rgb '#4169E1', \\\n" data-file))
        (insert (format "     '%s' using ($0):3 with linespoints axes x1y2 title 'Spending' lw 2 pt 7 lc rgb '#FF4500'\n" data-file)))
      
      ;; Execute gnuplot
      (call-process "gnuplot" nil nil nil plot-file)
      
      ;; Insert into org buffer
      (insert "\n#+ATTR_ORG: :width 800\n")
      (insert (format "[[file:%s]]\n\n" (file-relative-name image-file output-dir)))
      (insert (format "Generated from gnuplot script: [[file:%s]]\n" (file-relative-name plot-file output-dir)))
      (insert (format "Data file: [[file:%s]]\n" (file-relative-name data-file output-dir))))))

(defun bank-buddy-generate-subscriptions ()
  "Generate recurring subscriptions section."
  (let ((subscriptions '())
        (monthly-total 0))

    ;; Find likely subscriptions based on recurrence patterns
    (maphash
     (lambda (key occurrences)
       (when (>= (length occurrences) bank-buddy-core-subscription-min-occurrences)
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
      (let* ((avg-interval (/ (apply #'+ intervals) (float (length intervals))))
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

(defun bank-buddy-view-monthly-plots ()
  "Open the directory containing the monthly breakdown plots."
  (interactive)
  (let* ((output-file (if (boundp 'output-file) output-file nil))
         (plot-dir
          (expand-file-name
           "bank-buddy-monthly-plots"
           (or bank-buddy-core-output-directory
               (if output-file (file-name-directory output-file)
                 default-directory)))))
    
    (if (file-directory-p plot-dir)
        (progn
          (when (fboundp 'dired)
            (dired plot-dir))
          (message "Monthly plots are in: %s" plot-dir))
      (message "Monthly plots directory not found: %s" plot-dir))))

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
        (insert "You may want to add specific patterns for these in `bank-buddy-core-cat-list-defines`.\n\n")
        (insert "Copy these to use for your regex development:\n\n")
        (insert "#+begin_src elisp\n")
        (insert ";; Add these patterns to bank-buddy-core-cat-list-defines\n")
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

(defun bank-buddy-generate-report-directory (output-file latest-date)
  "Generate a directory name for the report based on OUTPUT-FILE and LATEST-DATE."
  (let* ((output-dir (file-name-directory output-file))
         (base-name (file-name-base output-file))
         (date-prefix (if latest-date
                          (format-time-string "%Y-%m-%d" (date-to-time latest-date))
                        (format-time-string "%Y-%m-%d")))
         (report-dir-name (format "%s--%s" date-prefix base-name)))
    (expand-file-name report-dir-name output-dir)))

;; --- Main Entry Point ---

;;;###autoload
(defun bank-buddy-reformat-csv-date-and-data ()
  "Convert date from \"dd/mm/yyyy\" to \"yyyy-mm-dd\" and reformat CSV data lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((result-lines '())
          desc amount balance day month year reformatted-date)
      (while (re-search-forward "^\"\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)\",\"\\([^\"]*\\)\",\"\\(-?[0-9.]+\\)\",\"\\(-?[0-9.]+\\)\"" nil t)
        (setq day (match-string 1))
        (setq month (match-string 2))
        (setq year (match-string 3))
        (setq desc (match-string 4))
        (setq amount (match-string 5))
        (setq balance (match-string 6))
        (unless (string-empty-p desc) ; skip lines with empty description
          ;; reformat date
          (setq reformatted-date (format "%s-%02d-%02d" year (string-to-number month) (string-to-number day)))
          ;; remove quotes and format line
          (push (format "%s,%s,%s,%s" reformatted-date desc (replace-regexp-in-string "^-\\(.*\\)" "\\1" amount) balance) result-lines)))
      (erase-buffer)
      ;; insert reformatted lines
      (dolist (line (nreverse result-lines))
        (insert line "\n")))))

;;;###autoload
(defun bank-buddy-generate-report (csv-file output-file)
  "Generate financial report from CSV-FILE asynchronously and save to OUTPUT-FILE."
  (interactive "fInput CSV file: \nFOutput Org file: ")

  ;; Ensure async is available
  (unless (fboundp 'async-start)
    (error "Async.el library not found.  Please install and load it"))

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
        (let ((bank-buddy-core-exclude-large-txns ,bank-buddy-core-exclude-large-txns)
              (bank-buddy-core-large-txn-threshold ,bank-buddy-core-large-txn-threshold)
              (bank-buddy-core-subscription-min-occurrences ,bank-buddy-core-subscription-min-occurrences)
              (bank-buddy-core-cat-list-defines ',bank-buddy-core-cat-list-defines)
              (bank-buddy-core-subscription-patterns ',bank-buddy-core-subscription-patterns)
              (bank-buddy-core-category-names ',bank-buddy-core-category-names))
          
          ;; Call the worker function and include file paths in the result
          (let ((worker-result (bank-buddy--process-csv-async-worker ,csv-file)))
            ;; Add the file paths to the result plist
            (plist-put worker-result :csv-file ,csv-file)
            (plist-put worker-result :output-file ,output-file)
            worker-result))))
   
   ;; The callback function that uses file paths from the result
   (lambda (result)
     ;; Extract file paths from the result
     (let* ((csv-file (plist-get result :csv-file))
            (output-file (plist-get result :output-file))
            (worker-err (plist-get result :error))
            (latest-date (plist-get result :date-last))
            (report-dir (bank-buddy-generate-report-directory output-file latest-date)))
       
       (if worker-err
           (progn
             (message "Bank Buddy: Background processing failed: %s" worker-err)
             (display-warning 'bank-buddy (format "Background processing failed: %s" worker-err) :error))
         
         (progn
           (bank-buddy-show-progress "Generating report..." t)
           
           ;; Create the report directory
           (make-directory report-dir t)
           
           ;; Update output-file to be in the new directory
           (setq output-file (expand-file-name (file-name-nondirectory output-file) report-dir))

           ;; Clear previous global data
           (clrhash bank-buddy-cat-tot)
           (clrhash bank-buddy-merchants)
           (clrhash bank-buddy-monthly-totals)
           (clrhash bank-buddy-txn-size-dist)
           (clrhash bank-buddy-subs)
           (clrhash bank-buddy-daily-cumulative-totals)
           (clrhash bank-buddy-monthly-transaction-counts)
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
           (when (hash-table-p (plist-get result :monthly-transaction-counts))
             (maphash (lambda (k v) (puthash k v bank-buddy-monthly-transaction-counts))
                      (plist-get result :monthly-transaction-counts)))
           (when (hash-table-p (plist-get result :daily-cumulative-totals))
             (maphash (lambda (k v) (puthash k v bank-buddy-daily-cumulative-totals))
                      (plist-get result :daily-cumulative-totals)))
           
           ;; Get the unmatched transactions list
           (setq bank-buddy-unmatched-transactions (plist-get result :unmatched-transactions))
           
           ;; Generate the report content in a temp buffer
           (with-temp-buffer
         (org-mode)
         (insert "#+title: Financial Report (Bank Buddy)\n")
         (insert (format "#+subtitle: Data from %s\n" (file-name-nondirectory csv-file)))
         (insert (format "#+date: %s\n" (format-time-string "%F %T")))
         (insert "#+options: toc:1 num:nil\n")
         (insert "#+startup: inlineimages showall\n\n")
         
         ;; Now pass report-dir to all functions that need it:
         (bank-buddy-generate-summary-overview)
         (bank-buddy-generate-top-spending-categories report-dir)  ; Pass report-dir
         (bank-buddy-generate-monthly-spending)
         (bank-buddy-generate-monthly-transaction-counts report-dir)  ; Pass report-dir
         (bank-buddy-generate-monthly-categories-table report-dir)  ; Pass report-dir
         (bank-buddy-generate-monthly-category-breakdowns report-dir)
         (bank-buddy-generate-monthly-progress-comparison report-dir)
         (bank-buddy-generate-top-merchants report-dir)  ; Pass report-dir
         (bank-buddy-generate-subscriptions)
         (bank-buddy-generate-transaction-size-distribution)
         (bank-buddy-generate-unmatched-transactions)
         
         (write-region (point-min) (point-max) output-file nil 'quiet))
           
           (bank-buddy-show-progress
            (format "Report generated successfully: %s\nMonthly plots saved to: %s"
                    output-file report-dir) t)
           
           (when (yes-or-no-p (format "Open generated report %s now?" output-file))
             (find-file output-file))))))))

;;;###autoload
(defun bank-buddy-generate ()
  "Intelligently generate a financial report based on context."
  (interactive)
  (let (input-file output-file)
    (cond
     ;; Case 1: In a CSV buffer
     ((and buffer-file-name (string-match-p "\\.csv$" buffer-file-name))
      (setq input-file buffer-file-name)
      (setq output-file (concat (file-name-sans-extension buffer-file-name) "_report.org")))
     
     ;; Case 2: In Dired
     ((eq major-mode #'dired-mode)
      (let ((file (dired-get-filename nil t)))
        (if (and file (string-match-p "\\.csv$" file))
            (progn
              (setq input-file file)
              (setq output-file (concat (file-name-sans-extension file) "_report.org")))
          (error "No CSV file selected in Dired"))))
     
     ;; Case 3: Neither in CSV buffer nor in Dired
     (t
      (error "Not in a CSV buffer or Dired.  Please open a CSV file or use Dired to select one")))
    
    ;; Generate the report
    (if (and input-file output-file)
        (progn
          (message "Generating report from %s" input-file)
          (bank-buddy-generate-report input-file output-file))
      (error "Failed to determine input and output files"))))

(provide 'bank-buddy)

;;; bank-buddy.el ends here
