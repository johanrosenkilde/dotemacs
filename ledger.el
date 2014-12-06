(setq jsrn-ledger-commodities (list "kr" "EUR"))
(setq ledger-convert-commodity "kr")

(setq ledger-run "ledger --strict --market -f %(ledger-file)")
(defun jsrn-ledger-report (spec)
  (concat ledger-run " " spec))
(setq ledger-reports (list 
        (list "bal"     (jsrn-ledger-report "bal ^Assets"))
        (list "reg"     (jsrn-ledger-report "reg"))
        (list "payee"   (jsrn-ledger-report "reg @%(payee)"))
        (list "account" (jsrn-ledger-report "reg %(account)"))
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

(defun ledger-import-csv (file-name)
  "Import a CSV file into the current (ledger) buffer.

  Go through the CSV file, line-by-line, assuming the following format
        DATE    TEXT    AMOUNT
  Query for the debit account for each transaction. Insert the result
  into this buffer."
  (interactive "fCSV file: ")
  (let* ((target-buf (current-buffer))
         (date-regex "\\([-/.0-9]+\\)")
         (name-regex "\\(.*\\)")
         (value-regex "\\(\\(-?[.,0-9]+\\)\\|\\(\"\\(-?[.,0-9]+\\)\"\\)\\)")
         (end-regex "[ \\t]?")
         (sep-regex "[ ,;\\t]+")
         (line-regex (concat "^" end-regex date-regex sep-regex name-regex sep-regex value-regex end-regex "$")))
    (with-temp-buffer
      (let ((source-buf (current-buffer)))
        (insert-file-contents file-name)
        (goto-char (point-min))
        (unless (looking-at line-regex)
          (error "First line of file does not match regex"))
        (let* ((accounts (save-excursion (set-buffer target-buf) (jsrn-ledger-find-accounts-in-buffer)))
               (accounts-copy (copy-list  accounts))
               (ido-common-match-string "")
               (credit (ido-completing-read "Credit account: " accounts-copy))
               (commodity (ido-completing-read "Commodity: " jsrn-ledger-commodities)))
          (while (not (eq (point) (point-max)))
            (if (looking-at line-regex)
                (let* ((raw-date (match-string 1)) ;; extract all matches before further regexp
                       (text (match-string 2))
                       (raw-val (if (match-string 4) (match-string 4) (match-string 6)))
                       (date (ledger-convert-date raw-date))
                       (val  (concat raw-val " " commodity))
                       (accounts-copy accounts)
                       (debit (ido-completing-read
                               (format "Transaction: %s,   amount %s\t(Press C-j for ignore transaction)\nDebit account: " text val)
                               accounts-copy)))
                  (unless (string-equal "" debit)
                    (set-buffer target-buf)
                    (insert (concat date " " text "\n    " credit "\t\t\t" val "\n    " debit "\n\n"))
                    (set-buffer source-buf))
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


;; (let ((url (concat
;;             "http://dict.tu-chemnitz.de/dings.cgi?lang=en&service=deen&opterrors=0&optpro=0&query="
;;             word "&iservice=&comment=&email="))
;;       (htmlfile (concat temporary-file-directory "beo_" word ".html"))
;;       (outbuffer "*beolingus*"))
;;   (if (not (file-exists-p htmlfile))
;;       (url-copy-file  url htmlfile))
;;   (shell-command (concat "vilistextum -w 10000 -u " htmlfile " -") outbuffer)
;;   (save-excursion
;;     (set-buffer outbuffer)
;;     (goto-char (point-min))
;;                                         ; Cut the beginning cruft
;;     (search-forward-regexp "[:digit:]* \\(similar \\)?results? for")
;;     (previous-line)
;;     (delete-region (point-min) (point))
;;                                         ; Kill link remainders and trim line beginnings
;;     (while (re-search-forward "\\[[^][]*\\]\\|\\[\\[[^[]*\\]\\]\\|^ +" nil t)
;;       (replace-match ""))
;;                                         ; Kill empty lines
;;     (goto-char (point-min))
;;     (delete-blank-lines)
;;     (while (not (eq (point) (point-max)))
;;       (forward-line)
;;       (delete-blank-lines))
;;     (goto-char (point-min))
;;     ;;Kill some cruft
;;     (forward-line)
;;     (kill-line) (kill-line) (kill-line) (kill-line)
;;                                         ;Find some end
;;     (let ((begin (point))
;;           (end (progn (forward-line 20) (point))))
;;       (table-capture begin end "   " "\n" "l" "40")
;;       )
;;     )
;;   )

(defun jsrn-ledger-mode ()
  (make-local-variable 'block-delimiter)
  (setq block-delimiter ";;;;")
  (fill-keymap ledger-mode-map
               (kbd "M-[") 'backward-block
               (kbd "M-]") 'forward-block
               )
  )
(add-hook 'ledger-mode-hook 'jsrn-ledger-mode)
