(defun julehjerte-sale-reply-hook ()
  (let* ((replybuf (current-buffer))
         (dir "~/julehjerter/company/web_salg/")
         (subject (plist-get mu4e-compose-parent-message :subject))
         (client (replace-regexp-in-string "^.*: " "" subject))
         (isdigital (string-match "Skabelon-Køb.*" subject))
         (emailtitle  (if isdigital "Skabeloner" "Bestilling"))
         (sent-maildir (if isdigital "/johansjulehjerter/Salg.Sendt" "/johansjulehjerter/Salg.Bekraeftet"))
         (boilerplate-file
          (if isdigital
              "digitalt_salg_svar.txt"
            "fysisk_ordrebekræftelse.txt"))
         (boilerplate (find-file (concat dir boilerplate-file))))
    ; Insert the boilerplate text in the mail
    (set-buffer replybuf)
    (insert-buffer-substring boilerplate)
    (kill-buffer boilerplate)
    ; Change the subject
    (goto-char (point-min))
    (re-search-forward "Subject: .*$" nil t)
    (replace-match (concat "Subject: Johans Julehjerter " emailtitle ": " julehjerte-current-orderno))
    ; Insert the client's name
    (while (re-search-forward "KUNDE" nil t)
      (replace-match client t))
    ; Optionally attach the bill
    (goto-char (point-max))
    (if isdigital
        (progn
          (mml-attach-file (concat dir julehjerte-current-orderno ".pdf") nil nil "attachment")
          (define-key evil-normal-state-local-map (kbd "C-c C-a")
            (lambda () (interactive)
              (jsrn-attach-multiple-files-dired "~/julehjerter/products/download_diagrams/")))
          ))
    (goto-char (point-min))
    ; Make sure the sent mail gets correctly stored
    (make-local-variable 'julehjerte~local-sent-maildir)
    (setq julehjerte~local-sent-maildir sent-maildir)
    ; Return mu4e-compose to normal
    (remove-hook 'mu4e-compose-mode-hook 'julehjerte-sale-reply-hook)
    ))

(defun julehjerte-make-faktura ()
  (interactive)
  (let* ((mail mu4e~view-msg)
         (mailfile (plist-get mail :path))
         (tmpbuf  "*skabelonsalg*")
         (dir "~/julehjerter/company/web_salg/")
         )
    (shell-command (concat dir "skabelon_salg.py " mailfile) tmpbuf)
    (let* ((orderno (with-current-buffer tmpbuf (current-word)))
           (basefile (concat dir "tex/" orderno)))
      (setq julehjerte-current-orderno orderno)
      (let ((fakturabuf (find-file (concat basefile ".tex"))))
        (with-current-buffer fakturabuf
          (define-key evil-normal-state-local-map (kbd "C-c C-c") 'exit-recursive-edit)
          (TeX-view)
          (recursive-edit))
        (kill-buffer fakturabuf)
        (shell-command (concat "cp " dir "tex/" orderno ".pdf " dir)))
      ; To run commands in the reply mail, we need to make a compose hook which
      ; then removes itself as a hook after being run.
      (add-hook 'mu4e-compose-mode-hook 'julehjerte-sale-reply-hook)
      (mu4e-compose-reply)
      )))
