(require 'ledger-mode)

(setq jsrn-ledger-commodities (list "kr" "EUR"))
(setq ledger-convert-commodity "kr")

(setq ledger-run "./myledger")
(defun jsrn-ledger-report (spec)
  (concat ledger-run " " spec))
(setq ledger-reports (list 
        (list "bal"     (jsrn-ledger-report "bal ^Assets ^Liabil ^Henlæg ^Moms"))
        (list "reg"     (jsrn-ledger-report "reg"))
        (list "payee"   (jsrn-ledger-report "reg @%(payee)"))
        (list "account" (jsrn-ledger-report "reg %(account)"))
        (list "valutatab" (jsrn-ledger-report "--daily reg ^Expenses:ValutaTab"))
        ))

(defun jsrn-ledger-find-accounts-in-buffer ()
  (let ((origin (point))
        accounts
        (seed-regex (ledger-account-any-status-with-seed-regex "")))
  (save-excursion
    (goto-char (point-min))
    (delete-dups
     (progn
       (while (re-search-forward seed-regex nil t)
         (unless (between origin (match-beginning 0) (match-end 0))
           (setq accounts (cons (match-string-no-properties 2) accounts))))
       accounts)))))
  
(defun ledger-convert-date (date)
  (if (string-match
  "\\([0-9][0-9]\\).\\([0-9][0-9]\\).\\([0-9][0-9][0-9][0-9]\\)"
  date)
      (concat (match-string 3 date)
              "/" (match-string 2 date)
              "/" (match-string 1 date))
    (error (concat  "Date doesn't conform to spec: " date))
      )
  )

(defun ledger-align ()
  "Align values of expenses in a region of a Ledger file."
  (interactive)
  (save-excursion
    (let ((endp (region-end)))
      (goto-char (region-beginning))
      (forward-line 0) ; mv to beginning of line
      (unless (looking-at "$")
        (backward-paragraph))
      (while (< (point) endp)
        (forward-line 2)
        (message "Doit %s" (point))
        (while (not (looking-at "$"))
          (if (looking-at "[ \t]*\\([[:alnum:]: /-]*\\)[ \t]\\{2,\\}\\(=?+?-?[0-9]+.?[0-9]*.?[0-9]*\\) \\(\\w*\\)$")
              (let* ((payee (match-string 1))
                     (amount (match-string 2))
                     (currency (match-string 3))
                     (begin (concat "    " payee))
                     (spaces (max 0 (- 90 (+ (length begin) (length amount))))))
                (kill-line)
                (insert begin (make-string spaces ? ) amount " " currency)
                )
            (forward-line))
          )
        )
    ))
)

(defun ledger-import-csv (file-name)
  "Import a CSV file into the current (ledger) buffer.

  Go through the CSV file, line-by-line, assuming the following format
        DATE    TEXT    AMOUNT
  Query for the debit account for each transaction. Insert the result
  into this buffer."
  (interactive "fCSV file: ")
  (let* ((target-buf (current-buffer))
         (date-regex "\\([-/.0-9]+\\)")
         (name-regex "\\(.*?\\)") ;; non-greedy all-match
         (value-regex
            (let ((number "+?\\(-?[0-9.,]+[,.][0-9]+\\)"))
              (concat "\\(" number "\\|\\(\"" number "\"\\)\\)")))
         (currency-regex " ?\\(EUR\\|kr\\)?")
         (end-regex "[; \t]*")
         (sep-regex "[ ,;\t]+")
         (line-regex (concat "^" end-regex date-regex sep-regex name-regex sep-regex value-regex currency-regex end-regex "$")))
    (with-temp-buffer
      (let ((source-buf (current-buffer)))
        (insert-file-contents file-name)
        (goto-char (point-min))
        (unless (looking-at line-regex)
          (error "First line of file does not match regex"))
        (setq last-debit nil)
        (let* ((accounts (save-excursion (set-buffer target-buf) (jsrn-ledger-find-accounts-in-buffer)))
               (ido-common-match-string "")
               (credit (ido-completing-read "Credit account: " (copy-list  accounts)))
               (commodity (ido-completing-read "Commodity: " jsrn-ledger-commodities)))
          (while (not (eq (point) (point-max)))
            (if (looking-at line-regex)
                (let* ((raw-date (match-string 1)) ;; extract all matches before further regexp
                       (text (match-string 2))
                       (raw-val (if (match-string 4) (match-string 4) (match-string 6)))
                       (date (ledger-convert-date raw-date))
                       (val  (concat raw-val " " commodity))
                       (debit (ido-completing-read
                               (format "Transaction: %s,   amount %s\t(Press C-j for ignore transaction)\nDebit account: " text val)
                               (copy-list accounts)  nil nil nil nil last-debit))
                       (nspace (- 90 (+ 4 (length credit) (length raw-val)))))
                  (unless (string-equal "" debit)
                    (set-buffer target-buf)
                    (insert date " " text "\n" "    " credit (make-string nspace ? ) val "\n    " debit "\n\n")
                    (set-buffer source-buf)
                    (setq last-debit debit))
                  (forward-line))
              (error "Line %d not well-formatted" (line-number-at-pos))
              )
            )
          )))))

(defun ledger-convert-buffer (credit)
  "Convert current buffer filled with tab-separated list of expenses to Ledger transactions"
  (interactive "sCredit account: ")
  ;; (interactive "sDebit account: ")
  (goto-char (point-min))
  (while (not (eq (point) (point-max)))
    (if (re-search-forward
         "^\\([.0-9]+\\)[[:space:]]+\\(.*\\)[[:space:]]+\\(-?[.,0-9]+\\)$")
        (progn
          (let* ((raw-date (match-string 1)) ;; extract all matches before further regexp
                 (text (match-string 2))
                 (raw-val (match-string 3))
                 (date (ledger-convert-date raw-date))
                 (val  (ledger-convert-value raw-val)))
            (kill-whole-line)
            (insert (concat date " " text "\n\t" credit "\t\t" val "\n\n"))
            )
          )
      (error "Line %d not well-formatted: " (line-number-at-pos))
      )
    ))


(defun jsrn-ledger-mode ()
  (make-local-variable 'block-delimiter)
  (setq block-delimiter ";;;;")
  (setq tab-width 4)
  (fill-keymap ledger-mode-map
               (kbd "M-[") 'backward-block
               (kbd "M-]") 'forward-block
               )
  )
(add-hook 'ledger-mode-hook 'jsrn-ledger-mode)


(message "Loaded ledger_setup.el")
(provide 'ledger_setup)
