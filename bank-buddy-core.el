;;; bank-buddy-core.el --- Financial analysis and reporting for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 James Dyer
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1") (async "1.9.4"))
;; Keywords: finance, budget, reporting
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
