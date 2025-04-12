;;; bank-buddy-core.el --- Financial analysis and reporting -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 James Dyer
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.1.1
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
;; Core part of bank-buddy
;;
;;; Code:

(defgroup bank-buddy nil
  "Customization options for bank-buddy."
  :group 'applications)

(defcustom bank-buddy-exclude-large-txns t
  "Whether to exclude transactions."
  :type 'boolean
  :group 'bank-buddy)

(defcustom bank-buddy-large-txn-threshold 2000
  "Threshold for large transactions in pounds."
  :type 'number
  :group 'bank-buddy)

(defcustom bank-buddy-subscription-min-occurrences 3
  "Minimum occurrences for subscription detection."
  :type 'number
  :group 'bank-buddy)

(defcustom bank-buddy-top-spending-categories 10
  "Number of top number of spending categories displayed."
  :type 'number
  :group 'bank-buddy)

(defcustom bank-buddy-top-merchants 10
  "Number of top number of merchants displayed."
  :type 'number
  :group 'bank-buddy)

(defcustom bank-buddy-monthly-spending-bar-width 80
  "Length of the bar in characters of Monthly Spending Features."
  :type 'number
  :group 'bank-buddy)

(defcustom bank-buddy-monthly-spending-max-bar-categories 6
  "Limit number of categories to keep visual clean."
  :type 'number
  :group 'bank-buddy)

(defcustom bank-buddy-output-directory nil
  "Directory to save report images.  If nil, uses the directory of the output file."
  :type '(choice (const :tag "Use output file directory" nil)
                 (directory :tag "Custom directory"))
  :group 'bank-buddy)

(defvar bank-buddy-unmatched-transactions-local '()
  "List of transactions that matched only the catch-all pattern.")

(defvar bank-buddy-unmatched-transactions '()
  "List of transactions that matched only the catch-all pattern.")

(defvar bank-buddy-highest-month-amount 0
  "The highest month amount.")

(defvar bank-buddy-payments '()
  "List of parsed payment transactions.  Populated by async callback.")

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

;; Category mappings
(defcustom bank-buddy-cat-list-defines
  '(("katherine\\|james\\|kate" "prs")
    ("railw\\|railway\\|train" "trn")
    ("paypal" "pay")
    ("electric\\|energy\\|water" "utl")
    ("racing" "bet")
    ("pension" "pen")
    ("savings\\|saver" "sav")
    ("uber" "txi")
    ("magazine\\|news" "rdg")
    ("claude\\|reddit\\|mobile\\|backmarket\\|openai\\|web" "web")
    ("notemachine\\|withdrawal" "atm")
    ("finance" "fin")
    ("youtube\\|netflix" "str")
    ("card" "crd")
    ("top-up\\|phone" "phn")
    ("amaz\\|amz" "amz")
    ("pets\\|pet" "pet")
    ("dentist" "dnt")
    ("residential\\|rent\\|mortgage" "hse")
    ("deliveroo\\|just.*eat" "fod")
    ("ebay\\|apple\\|itunes" "shp")
    ("law" "law")
    ("anyvan" "hmv")
    (".*" "o"))
  "Categorization patterns for transactions."
  :type '(alist :key-type string :value-type string)
  :group 'bank-buddy)

(defcustom bank-buddy-category-names
  '(("prs" . "Personal")
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
  "Human-readable category names for reporting."
  :type '(alist :key-type string :value-type string)
  :group 'bank-buddy)

(defcustom bank-buddy-subscription-patterns
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
  "Patterns to identify specific subscriptions."
  :type '(alist :key-type string :value-type string)
  :group 'bank-buddy)

(provide 'bank-buddy-core)
;;; bank-buddy-core.el ends here
