;; Example usage of the bank-buddy package

;; 1. First, load the package
(use-package bank-buddy
  :load-path "~/source/repos/bank-buddy"
  :custom
  (bank-buddy-core-top-spending-categories 20)
  (bank-buddy-core-top-merchants 20)
  (bank-buddy-core-large-txn-threshold 1200)
  (bank-buddy-core-monthly-spending-bar-width 160)
  (bank-buddy-core-monthly-spending-max-bar-categories 20)
  (bank-buddy-core-cat-list-defines
   '(("katherine\\|james\\|kate" "prs")
    ("railw\\|railway\\|train" "trn")
    ("paypal" "pay")
    ("virgin-media\\|electric\\|energy\\|water" "utl")
    ("bat365\\|racing\\|betting" "bet")
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
    ("sainsbury\\|tesco\\|deliveroo\\|just.*eat\\|asda" "fod")
    ("river-island\\|ebay\\|apple\\|itunes" "shp")
    ("law" "law")
    ("anyvan" "hmv")
    (".*" "o"))))

;; 2. Generate a financial report from a CSV file
(bank-buddy-generate-report "financial-report.csv" "financial-report.org")
