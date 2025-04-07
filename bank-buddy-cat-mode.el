;;; bank-buddy-cat-mode.el --- Category enhancement mode for bank-buddy -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your-email@example.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (bank-buddy "1.1") (async "1.9.4"))
;; Keywords: extensions

;;; Commentary:
;;
;; This package provides an interactive mode for managing transaction
;; categorization in the bank-buddy package. It allows users to:
;;
;; - Quickly select unmatched transactions from a report
;; - Add them to the category definitions
;; - Choose existing categories or create new ones
;; - Regenerate reports to see the effects of changes
;;
;; The mode is particularly useful when iteratively improving the
;; categorization of bank transactions.

;;; Code:

;; (require 'bank-buddy)
(require 'org)
(require 'cl-lib)

;; Convert bank-buddy-cat-list-defines to defcustom
(defcustom bank-buddy-cat-list-defines
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
  "Categorization patterns for transactions.
Each entry is a cons cell where the car is a regex pattern
and the cdr is the category code to assign when the pattern matches."
  :type '(repeat (list (string :tag "Pattern Regex")
                       (string :tag "Category Code")))
  :group 'bank-buddy)

;; Variables for the mode
(defvar bank-buddy-cat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") 'bank-buddy-cat-add-pattern)
    (define-key map (kbd "C-c C-r") 'bank-buddy-cat-regenerate-report)
    map)
  "Keymap for Bank Buddy Category mode.")

(defvar-local bank-buddy-cat-original-csv-file nil
  "The original CSV file used to generate the current report.")

(defvar-local bank-buddy-cat-original-report-file nil
  "The original report file (current buffer).")

(defvar bank-buddy-cat-last-category nil
  "The last category used, for convenience.")

;;;###autoload
(define-minor-mode bank-buddy-cat-mode
  "Minor mode for managing bank-buddy transaction categories.

This mode allows you to quickly add unmatched transactions to the
category definitions and regenerate reports to see the effects.

\\{bank-buddy-cat-mode-map}"
  :init-value nil
  :lighter " BB-Cat"
  :keymap bank-buddy-cat-mode-map
  :group 'bank-buddy
  (if bank-buddy-cat-mode
      (bank-buddy-cat-mode-setup)
    (bank-buddy-cat-mode-teardown)))

(defun bank-buddy-cat-mode-setup ()
  "Setup for bank-buddy-cat-mode."
  (message "Bank Buddy Category mode enabled. Use C-c C-a to add patterns, C-c C-r to regenerate report."))

(defun bank-buddy-cat-mode-teardown ()
  "Teardown for bank-buddy-cat-mode."
  (message "Bank Buddy Category mode disabled."))

(defun bank-buddy-cat-detect-csv-file ()
  "Try to detect the CSV file that was used to generate this report."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Data from \\(.+\\)$" nil t)
      (let ((filename (match-string 1)))
        ;; Check if this is just a filename or a full path
        (if (file-name-absolute-p filename)
            filename
          ;; Try to find it in the same directory as the report
          (let ((dir (file-name-directory (buffer-file-name))))
            (expand-file-name filename dir)))))))

(defun bank-buddy-cat-get-existing-categories ()
  "Get a list of existing category codes."
  (mapcar (lambda (item) (nth 1 item)) bank-buddy-cat-list-defines))

(defun bank-buddy-cat-get-category-name (code)
  "Get the human-readable name for category CODE."
  (or (cdr (assoc code bank-buddy-category-names))
      code))

(defun bank-buddy-cat-choose-category (pattern)
  "Let the user choose a category for PATTERN.
Returns the selected category code."
  (let* ((categories (bank-buddy-cat-get-existing-categories))
         (category-choices
          (mapcar (lambda (code)
                    (cons (format "%s (%s)" 
                                 (bank-buddy-cat-get-category-name code)
                                 code)
                          code))
                  (delete-dups categories)))
         (initial (or bank-buddy-cat-last-category (car categories)))
         (completion-extra-properties
          '(:annotation-function
            (lambda (cat)
              (let ((code (cdr (assoc cat category-choices))))
                (format " - %s" (bank-buddy-cat-get-category-name code))))))
         (choice (completing-read 
                  (format "Category for \"%s\" [%s]: " 
                          pattern 
                          (bank-buddy-cat-get-category-name initial))
                  category-choices
                  nil ; predicate
                  nil ; require-match
                  nil ; initial-input
                  nil ; hist
                  initial ; def
                  ))
         (category-code (or (cdr (assoc choice category-choices))
                            (if (y-or-n-p (format "Create new category '%s'? " choice))
                                (read-string "Enter category code (short, e.g. 'fod'): ")
                              (user-error "Category selection canceled")))))
    
    ;; Remember this choice for next time
    (setq bank-buddy-cat-last-category category-code)
    category-code))

(defun bank-buddy-cat-add-pattern ()
  "Add the selected transaction pattern to category definitions.
This command should be used when the point is in an unmatched transaction
in the bank-buddy report."
  (interactive)
  
  ;; Check if we're in a bank-buddy report
  (unless (derived-mode-p 'org-mode)
    (user-error "This command should be used in an org-mode bank-buddy report"))
  
  ;; Get selected text or current line if no selection
  (let ((pattern (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-substring-no-properties 
                   (line-beginning-position) 
                   (line-end-position)))))
    
    ;; Clean up the pattern - remove any leading/trailing whitespace or quotes
    (setq pattern (string-trim pattern))
    (when (string-match "^\"\\(.*\\)\"$" pattern)
      (setq pattern (match-string 1 pattern)))
    
    ;; Confirm with the user
    (unless (y-or-n-p (format "Add pattern \"%s\" to category definitions? " pattern))
      (user-error "Operation canceled"))
    
    ;; Ask for category
    (let ((category (bank-buddy-cat-choose-category pattern)))
      
      ;; Escape regex special characters in the pattern
      (setq pattern (regexp-quote pattern))
      
      ;; Add the new pattern to bank-buddy-cat-list-defines
      (let* ((existing-categories (bank-buddy-cat-get-existing-categories))
             (category-entry (assoc category existing-categories))
             (catch-all-entry (assoc ".*" bank-buddy-cat-list-defines))
             (position-to-insert (if catch-all-entry
                                     (- (length bank-buddy-cat-list-defines) 1)
                                   (length bank-buddy-cat-list-defines))))
        
        ;; Insert the new pattern before the catch-all pattern
        (let ((new-defines (copy-sequence bank-buddy-cat-list-defines)))
          (setf (nthcdr position-to-insert new-defines)
                (cons (list pattern category)
                      (nthcdr position-to-insert new-defines)))
          (setq bank-buddy-cat-list-defines new-defines))
        
        ;; Confirm the operation
        (message "Added pattern \"%s\" to category \"%s\" (%s)"
                 pattern
                 (bank-buddy-cat-get-category-name category)
                 category)
        
        ;; Ask if the user wants to save to init file
        (when (y-or-n-p "Save this category definition to your init file? ")
          (bank-buddy-cat-save-to-init-file))
        
        ;; Ask if the user wants to regenerate the report
        (when (y-or-n-p "Regenerate the report to see changes? ")
          (bank-buddy-cat-regenerate-report))))))

(defun bank-buddy-cat-save-to-init-file ()
  "Save the current bank-buddy-cat-list-defines to the user's init file."
  (interactive)
  
  ;; Determine which file to save to
  (let* ((init-file (or user-init-file "~/.emacs"))
         (file-to-use 
          (read-file-name 
           "Save to init file: " 
           (file-name-directory init-file)
           nil nil
           (file-name-nondirectory init-file))))
    
    ;; If the file doesn't exist, create it
    (unless (file-exists-p file-to-use)
      (when (y-or-n-p (format "File %s doesn't exist. Create it? " file-to-use))
        (write-region "" nil file-to-use)))
    
    ;; Check that the file exists
    (if (not (file-exists-p file-to-use))
        (user-error "Cannot save to non-existent file %s" file-to-use)
      
      ;; Generate the elisp code to set the custom variable
      (let ((elisp-code 
             (format "(customize-set-variable 'bank-buddy-cat-list-defines\n  '%S)\n"
                     bank-buddy-cat-list-defines)))
        
        ;; Handle different cases for how to insert the code
        (with-current-buffer (find-file-noselect file-to-use)
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "(customize-set-variable 'bank-buddy-cat-list-defines" nil t)
                ;; Replace existing definition
                (let ((start (match-beginning 0)))
                  (re-search-forward ")")
                  (forward-line)
                  (delete-region start (point))
                  (goto-char start)
                  (insert elisp-code))
              ;; Add new definition at the end
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "\n;; Bank Buddy category definitions\n")
              (insert elisp-code)))
          (save-buffer)
          (message "Category definitions saved to %s" file-to-use))))))

(defun bank-buddy-cat-regenerate-report ()
  "Regenerate the bank-buddy report using the current CSV file."
  (interactive)
  
  ;; Check if we're in a bank-buddy report
  (unless (derived-mode-p 'org-mode)
    (user-error "This command should be used in an org-mode bank-buddy report"))
  
  ;; Detect the CSV file if not already set
  (unless bank-buddy-cat-original-csv-file
    (setq bank-buddy-cat-original-csv-file (bank-buddy-cat-detect-csv-file)))
  
  ;; Make sure we have a CSV file
  (unless (and bank-buddy-cat-original-csv-file
               (file-exists-p bank-buddy-cat-original-csv-file))
    (setq bank-buddy-cat-original-csv-file
          (read-file-name "CSV file to process: " nil nil t)))
  
  ;; Make sure we have an output file
  (unless bank-buddy-cat-original-report-file
    (setq bank-buddy-cat-original-report-file (buffer-file-name)))
  
  (when (and bank-buddy-cat-original-csv-file
             bank-buddy-cat-original-report-file)
    
    ;; Confirm the operation
    (when (y-or-n-p (format "Regenerate report from %s? "
                            (file-name-nondirectory bank-buddy-cat-original-csv-file)))
      
      ;; Remember the position
      (let ((pos (point)))
        
        ;; Generate the report
        (bank-buddy-generate-report 
         bank-buddy-cat-original-csv-file
         bank-buddy-cat-original-report-file)
        
        ;; Refresh the buffer
        (revert-buffer t t t)
        
        ;; Restore position approximately
        (goto-char (min pos (point-max)))))))

;; Helper function to detect report properties when enabling the mode
(defun bank-buddy-cat-detect-report-properties ()
  "Try to detect report properties from the current buffer."
  (save-excursion
    (goto-char (point-min))
    
    ;; Detect the CSV file
    (when (re-search-forward "Data from \\(.+\\)$" nil t)
      (let ((filename (match-string 1)))
        ;; Check if this is just a filename or a full path
        (setq bank-buddy-cat-original-csv-file
              (if (file-name-absolute-p filename)
                  filename
                ;; Try to find it in the same directory as the report
                (let ((dir (file-name-directory (buffer-file-name))))
                  (expand-file-name filename dir))))))
    
    ;; Set the report file
    (setq bank-buddy-cat-original-report-file (buffer-file-name))))

;; Auto-enable for bank-buddy reports
(defun bank-buddy-cat-maybe-enable ()
  "Enable bank-buddy-cat-mode if this looks like a bank-buddy report."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "Financial Report (Bank Buddy)" nil t)))
    (bank-buddy-cat-detect-report-properties)
    (bank-buddy-cat-mode 1)))

;; Add hook to auto-enable
(add-hook 'org-mode-hook 'bank-buddy-cat-maybe-enable)

(provide 'bank-buddy-cat-mode)
;;; bank-buddy-cat-mode.el ends here
