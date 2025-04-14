;;; bank-buddy-cat-mode.el --- Category enhancement mode for bank-buddy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 James Dyer
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.1.1
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

(require 'org)
(require 'cl-lib)
(require 'bank-buddy-core)

(declare-function bank-buddy-generate-report "bank-buddy")

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
  "Setup for `bank-buddy-cat-mode'."
  (message "Bank Buddy Category mode enabled. Use C-c C-a to add patterns, C-c C-r to regenerate report."))

(defun bank-buddy-cat-mode-teardown ()
  "Teardown for `bank-buddy-cat-mode'."
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
  (mapcar (lambda (item) (nth 1 item)) bank-buddy-core-cat-list-defines))

(defun bank-buddy-cat-get-category-name (code)
  "Get the human-readable name for category CODE."
  (or (cdr (assoc code bank-buddy-core-category-names))
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
  (save-excursion
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

      (deactivate-mark)
      
      ;; Confirm with the user
      (unless (y-or-n-p (format "Add pattern \"%s\" to category definitions? " pattern))
        (user-error "Operation canceled"))
      
      ;; Ask for category
      (let ((category (bank-buddy-cat-choose-category pattern)))
        
        ;; Escape regex special characters in the pattern
        (setq pattern (regexp-quote pattern))
        
        ;; Add the new pattern to bank-buddy-core-cat-list-defines
        (let* ((catch-all-entry (assoc ".*" bank-buddy-core-cat-list-defines))
               (position-to-insert (if catch-all-entry
                                       (- (length bank-buddy-core-cat-list-defines) 1)
                                     (length bank-buddy-core-cat-list-defines))))
          
          ;; Insert the new pattern before the catch-all pattern
          (let ((new-defines (copy-sequence bank-buddy-core-cat-list-defines)))
            (setf (nthcdr position-to-insert new-defines)
                  (cons (list pattern category)
                        (nthcdr position-to-insert new-defines)))
            (setq bank-buddy-core-cat-list-defines new-defines))
          
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
            (bank-buddy-cat-regenerate-report)))))))

(defun bank-buddy-cat-save-to-init-file ()
  "Save the current `bank-buddy-core-cat-list-defines' to the user's init file."
  (interactive)
  
  ;; Determine which file to save to
  (let* ((init-file (or user-init-file
                        (expand-file-name "init.el" user-emacs-directory)))
         (file-to-use
          (read-file-name
           "Save to init file: "
           (file-name-directory init-file)
           nil nil
           (file-name-nondirectory init-file))))
    
    ;; If the file doesn't exist, create it
    (unless (file-exists-p file-to-use)
      (when (y-or-n-p (format "File %s doesn't exist.  Create it? " file-to-use))
        (write-region "" nil file-to-use)))
    
    ;; Check that the file exists
    (if (not (file-exists-p file-to-use))
        (user-error "Cannot save to non-existent file %s" file-to-use)
      
      ;; Generate the elisp code to set the custom variable
      (let ((elisp-code
             (format "(customize-set-variable 'bank-buddy-core-cat-list-defines\n  '%S)\n"
                     bank-buddy-core-cat-list-defines)))
        
        ;; Handle different cases for how to insert the code
        (with-current-buffer (find-file-noselect file-to-use)
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "(customize-set-variable 'bank-buddy-core-cat-list-defines" nil t)
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
  (save-excursion
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
        (goto-char (min pos (point-max))))))))

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
  "Enable `bank-buddy-cat-mode' if this look like a bank-buddy report."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "Financial Report (Bank Buddy)" nil t)))
    (bank-buddy-cat-detect-report-properties)
    (bank-buddy-cat-mode 1)))

;; Add hook to auto-enable
;; (add-hook 'org-mode-hook #'bank-buddy-cat-maybe-enable)

(provide 'bank-buddy-cat-mode)
;;; bank-buddy-cat-mode.el ends here
