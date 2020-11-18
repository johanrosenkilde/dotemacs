;; Printing only rarely used so
;; OBS: THIS FILE IS NOT LOADED BY DEFAULT
;;
(require 'printing)		; load printing package
(setq jsrn-pr-printer-alist
      '((et2-006 "lpr"     nil "et2-color-konica-006")   ;; Ulm University
        ))
(setq jsrn-current-printer 'et2-006)
(defun jsrn-pr-set-printers ()
  (interactive)
  (setq pr-path-alist
        '((unix      "." ghostview mpage PATH)
          (ghostview "/usr/bin/gv")
          (mpage     "/usr/bin/mpage")
          ))
  (setq pr-txt-printer-alist jsrn-pr-printer-alist)
  (setq pr-ps-printer-alist jsrn-pr-printer-alist)
  (setq pr-txt-name  jsrn-current-printer)
  (setq pr-ps-name  jsrn-current-printer)
  (pr-update-menus t)		; update now printer and utility menus
)
(jsrn-pr-set-printers)
