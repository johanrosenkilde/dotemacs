;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           OFFLINEIMAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To delete trash email, use the following one-liner
;; $ find ~/mail/trash/cur -mtime +30 -type f -delete


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           MU4E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jsrn-smtpmail-setup (from-email)
  ;; tell message-mode how to send mail
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-queue-dir "~/mail/queued-mail/"
        smtpmail-debug-info t ; only to debug problems
        )
  (cond
   ((string-match ".*dtu.dk" from-email)
    (progn
       (setq
        smtpmail-smtp-server "smtpauth.compute.dtu.dk"
        smtpmail-smtp-user "jsrn"
        smtpmail-smtp-service 587
        smtpmail-local-domain nil
        smtpmail-stream-type 'starttls
        )
       (message "Using DTU SMTP")))
   ((string-match ".*johansjulehjerter.dk" from-email)
    (progn
      (setq
       smtpmail-smtp-server "asmtp.unoeuro.com"
       smtpmail-smtp-user "johan@johansjulehjerter.dk"
       smtpmail-smtp-service 587
       smtpmail-local-domain "johansjulehjerter.dk"
       smtpmail-stream-type 'starttls
       )
      (message "Using UnoEuro (Johansjulehjerter) SMTP")))
   ((string-match ".*gmail.com" from-email)
    (progn
      (setq
       smtpmail-smtp-server "smtp.gmail.com"
       smtpmail-smtp-user "santaphile@gmail.com"
       smtpmail-smtp-service 587
       smtpmail-local-domain nil
       smtpmail-stream-type 'starttls
       )
      (message "Using Gmail SMTP")))
   (t
    (progn
      (setq
       smtpmail-smtp-server "asmtp.unoeuro.com"
       smtpmail-smtp-user "jsrn@jsrn.dk"
       smtpmail-smtp-service 587
       smtpmail-local-domain "jsrn.dk"
       smtpmail-stream-type 'starttls
       )
      (message "Using UnoEuro (jsrn) SMTP")))
     ))
(jsrn-smtpmail-setup "jsrn")

(defun jsrn-attach-file (file &optional type)
  "Essentially call `mml-attach-file' but without description and always
  attached disposition."
  (interactive
    (let* ((file (mml-minibuffer-read-file "Attach file: "))
           (type (mml-minibuffer-read-type file)))
      (list file type)))
  (mml-attach-file file type nil "attachment")
)

(defun jsrn-attach-multiple-files-dired (dir)
   "Attach multiple files from same dir using dired.
   Called from within compose"
  (interactive "D")
  (dired-other-window dir)
  (turn-on-gnus-dired-mode)
  (define-key gnus-dired-mode-map (kbd "<C-return>")
    (lambda ()
      (interactive)
      (gnus-dired-attach (dired-get-marked-files))
      ))
  )

;; perl script delatt didn't work for some reason
;; (defun jsrn-delete-attachments ()
;;   "Delete all attachments of the current mail. If jsrn-delete-attacments-save is
;;   set to some path, the attachments will be saved to this folder."
;;   (interactive)
;;   (let ((msg (mu4e-message-at-point)))
;;     (shell-command-to-string "delatt
;;         (message (mu4e-message-field msg :path)))
;; )

(defun jsrn-mailto-from-kill ()
  "Parse the contents of the top of the kill ring as a mailto-link and apply it"
  (interactive)
  (setq uri (current-kill 0 t))
  (set-text-properties 0 (length uri) nil uri)
  (when (string-match "\".*\"" uri)
    (setq uri (substring uri 1 (- (length uri) 1))))
  (mu4e~compose-browse-url-mail uri))

(defun jsrn-view-message-expand-links ()
  "View a currently viewed message with links expanded"
  (interactive)
  ;; Add the -k flag to vilistextum and reread the message
  (setq old-command mu4e-html2text-command)
  (setq mu4e-html2text-command "vilistextum -k - -  | iconv -f \"iso-8859-1\" -t\"utf-8\" - -")
  (mu4e~view-in-headers-context (mu4e-headers-view-message))
  (sleep-for 0.2) ; avoid funny race condition
  (setq mu4e-html2text-command old-command)
)

;; Setup smtp mail information
(require 'auth-source)
(setq auth-sources '((:source "~/.authinfo.gpg")))

(add-to-list 'load-path"/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-compose-signature nil)
;; Handling html messages
(setq jsrn-html2text-commands (list "vilistextum -c -r - -  | iconv -f \"iso-8859-1\" -t\"utf-8\" - -"
                                     "html2text"
                                     "myhtml2text iso-8859-1"
                                     'mu4e-shr2text
                                    ))
(defun jsrn-switch-html2text ()
  (interactive)
  (setq mu4e-html2text-command
        (next-in-list jsrn-html2text-commands
                      mu4e-html2text-command))
  ;; refresh
  (with-current-buffer mu4e~headers-buffer-name
    (mu4e-headers-view-message)
    (message "Switched html2text to: '%s'" mu4e-html2text-command)
    )
  )
;;(jsrn-switch-html2text)
(define-key mu4e-view-mode-map [(f5)] 'jsrn-switch-html2text)
(setq mu4e-view-prefer-html nil)

(defun mu4e-view-in-browser ()
  "View the body of the message in a web browser."
  (interactive)
  (let (msg txt html)
    (setq msg (mu4e-message-at-point t))
    (setq txt (plist-get msg :body-txt))
    (setq html (plist-get msg :body-html))
    (with-temp-buffer
      (when (> (length html) 0)
        (insert html))
      (when (> (length txt) 0)
        (insert txt))
      (browse-url-of-buffer)))) 

;; Call view-in-browser when pressing "ab" (the leading b in str below is key)
(add-to-list 'mu4e-view-actions
             '("bView in browser" . mu4e-action-view-in-browser) t)

;; Set up the mailboxes for refiling, sent etc.
(defun jsrn-mu4e-mailbox (msg)
  (let ((maildir (mu4e-message-part-field msg :maildir)))
    (if (eq nil (string-match "^/\\([^/]*\\)/" maildir))
        (error "This maildir had invalid format: %s" maildir)
      (match-string 1 maildir)
      )))
(defun jsrn-set-sent-folder (msg)
  (if (boundp 'julehjerte~local-sent-maildir)
      ; Temporary: for use in julehjerte business
      julehjerte~local-sent-maildir
    (if (eq msg nil)
        (progn
          (message "Sent mail copied to default sent folder: %s" jsrn-mu4e-sent-folder)
          jsrn-mu4e-sent-folder)
      (let ((mailbox (jsrn-mu4e-mailbox msg)))
        (cond ((string-equal mailbox "jsrn") "/jsrn/Sent")
              ((string-equal mailbox "atuin") "/jsrn/Sent")
              ((string-equal mailbox "johansjulehjerter") "/johansjulehjerter/Sent")
              ((string-equal mailbox "mailinglist") "/mailinglist/INBOX.Sent")
              ((string-equal mailbox "dtu") "/dtu/Sent")
              ((string-equal mailbox "inria")   "/inria/Sent")
              ((string-equal mailbox "gmail") "/gmail/[Gmail].Sent Mail")
              (t jsrn-mu4e-sent-folder)))))
  )

(setq mu4e-maildir "~/mail"
      jsrn-mu4e-sent-folder "/jsrn/Sent"
      mu4e-sent-folder 'jsrn-set-sent-folder
      mu4e-drafts-folder "/jsrn/INBOX.Drafts"
      mu4e-trash-folder "/trash"
      mu4e-refile-folder (lambda (msg)
                           (let ((mailbox (jsrn-mu4e-mailbox msg))
                                 (maildir (mu4e-message-part-field msg :maildir))
                                 (year (format-time-string "%Y")))
                             (cond ((string-equal mailbox "jsrn") (concat "/jsrn/Archives." year))
                                   ((string-equal mailbox "atuin") (concat "/jsrn/Archives." year))
                                   ((string-equal maildir "/johansjulehjerter/Salg.Bekraeftet") "/johansjulehjerter/Salg.Sendt")
                                   ((string-equal mailbox "johansjulehjerter") "/johansjulehjerter/Archives")
                                   ((string-equal mailbox "mailinglist") "/mailinglist/INBOX.Archive")
                                   ((string-equal mailbox "gmail") "/gmail/[Gmail].All Mail")
                                   ((string-equal mailbox "dtu")   (concat "/dtu/Archives." year)))))
      )
(setq mu4e-attachment-dir "~/downloads/tmp")


;; Set up some shortcuts access them with 'j' ('jump')
(let* ((year (format-time-string "%Y"))
      (shortcuts `(("/jsrn/INBOX"       . ?i)
                    ("/jsrn/Sent"  . ?s)
                    ("/jsrn/INBOX.To Use". ?u)
                    ("/dtu/INBOX"         . ?I)
                    ("/dtu/Sent"          . ?S)
                    ("/jsrn/INBOX.To Use". ?u)
                    ("/jsrn/INBOX.Drafts". ?d)
                    ("/trash"             . ?w)
                    ("/johansjulehjerter/Salg.Sendt" . ?U)))
      (jsrn_arch (concat "/jsrn/Archives." year))
      (dtu_arch  (concat "/dtu/Archives." year)))
  (add-to-list 'shortcuts (cons dtu_arch ?A))
  (add-to-list 'shortcuts (cons jsrn_arch ?a))
  (setq mu4e-maildir-shortcuts shortcuts))
(add-to-list 'mu4e-bookmarks
   '("maildir:/jsrn/Archives.2019 or maildir:/jsrn/Archives.2018 or
maildir:/johansjulehjerter/Archives or maildir:/dtu/Archives.2018 or maildir:/dtu/Archives.2018" "Archives" ?a))
;; View the contents of private inboxes or sent with 'bi' or 'bs'
(add-to-list 'mu4e-bookmarks
   '("maildir:/jsrn/INBOX or maildir:/johansjulehjerter/INBOX or maildir:/gmail/INBOX"  "Inboxes"  ?i))
(add-to-list 'mu4e-bookmarks
   '("maildir:/jsrn/Sent or maildir/dtu/Sent or maildir:/Sent.Old or maildir:/johansjulehjerter/Sent or maildir:/mailinglist/Sent or maildir:/gmail/[Gmail].Sent Mail" "Sent" ?s))
;; (add-to-list 'mu4e-bookmarks
;;    '("maildir:/mailinglist/INBOX and date:1M.."  "Mailinglist"  ?n))

;; Set up the Header fields (only thread-subject differs from standard)
(setq mu4e-headers-fields
      '((:human-date . 12) (:flags . 6) (:mailing-list . 10) (:from . 22)
      (:thread-subject))
      )

;; Check mail using offlineimap every 5 min
(setq mu4e-get-mail-command "getmail"
      mu4e-update-interval 300
      mu4e-headers-auto-update nil
      mail-user-agent 'mu4e-user-agent
      )

;; Messages should be replied from the recipient if it was one of the
;; registered email addresses
(setq mu4e-user-mail-address-list '("jsrn@jsrn.dk"
                                    "jsrn@dtu.dk"
                                    "johan@johansjulehjerter.dk"
                                    "mailinglist@jsrn.dk"
                                    "bank@jsrn.dk"
                                    "santaphile@gmail.com"
                                    "atuin@atuin.dk"
                                    "spammy@jsrn.dk"
                                    ))

;; Switch my from address to the next possible from address
(defun next-from-address ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "<\\(.*\\)>")
    (let* ((oldmail (match-string 1))
          (email  (next-in-list mu4e-user-mail-address-list oldmail)))
      (replace-match email nil nil nil 1))
  )
)

(fill-keymap mu4e-compose-mode-map [(f2)] 'next-from-address)

(defun jsrn-set-from-address ()
  "Set the From address, and Sent Folder based on the To address of the original"
  (setq user-mail-address
        (let ((msg mu4e-compose-parent-message))
          (if msg
              ;; Take the destination addr of msg. Match whether it was a Google
              ;; group mail, or a mail to one of my email addresses.
              (let ((toaddr (cdr (car (mu4e-message-part-field msg :to)))))
                (if (eq toaddr nil)
                    jsrn-user-mail-address
                  (if (string-match ".*@googlegroups.com" toaddr)
                      "mailinglist@jsrn.dk"
                    (if (string-match "webshop@johansjulehjerter.dk" toaddr)
                        "webshop@johansjulehjerter.dk"
                      (if (member (downcase toaddr) mu4e-user-mail-address-list)
                          (downcase toaddr)
                        jsrn-user-mail-address)))))
            jsrn-user-mail-address)))
  )
(add-hook 'mu4e-compose-pre-hook 'jsrn-set-from-address)

(defun jsrn-send-mail-set-smtp ()
  (jsrn-smtpmail-setup (mail-fetch-field "from")))
(add-hook 'message-send-hook  'jsrn-send-mail-set-smtp)

;; flyspelling the email: flyspell-buffer croaks across the "text follows ..." line
(defun flyspell-body ()
  (interactive)
  (save-excursion
    (message-goto-body)
    (flyspell-region (point) (progn (end-of-buffer) (point)))))

;; Setup email writing
(defun jsrn-mu4e-compose-setup ()
  (flyspell-mode t)
  (setq ispell-local-dictionary "dansk")
  ;; Add footnote and bibtex/footnote support
  (footnote-mode t)
  (setq reftex-default-bibliography '("~/mat/tex/bibtex.bib"))
  (defun jsrn-insert-citation ()
    (interactive)
    (let ((reftex-cite-format "%a: %t. %j %v:%p, %y"))
        (reftex-citation)))
  (define-key mu4e-compose-mode-map (kbd "C-c B") 'jsrn-insert-citation)
  (fill-keymaps (list evil-normal-state-map evil-insert-state-map)
                (kbd "C-c ! c") (lambda ()
                                  (interactive)
                                  (Footnote-add-footnote)
                                  (jsrn-insert-citation))
                [(f6)]
                  (lambda () (interactive) (jsrn-cycle-dictionary) (flyspell-body))
                ;; Change attach file to my slightly quicker and nicer one
                (kbd "C-c C-a")  'jsrn-attach-file
                (kbd "C-c C-S-a") 'jsrn-attach-multiple-files-dired
                )
  )
(add-hook 'mu4e-compose-mode-hook 'jsrn-mu4e-compose-setup)

;; Don't ask on exiting
(setq mu4e-confirm-quit nil)

;; Close emails after sending them
(setq message-kill-buffer-on-exit t)

;; backspace should clear mark like in dired
(define-key mu4e-headers-mode-map (kbd "<backspace>")
  (lambda () (interactive)
    (mu4e-headers-prev)
    (mu4e-headers-mark-for-unmark)
    (mu4e-headers-prev)))

;; A better mark-for-move funtion which properly handles multiple messages in region
(define-key mu4e-headers-mode-map (kbd "m")
  (lambda () (interactive)
    (let ((target (mu4e~mark-get-move-target)))
      (mu4e-mark-set 'move target))))

;; Disable evil
(evil-set-initial-state 'mu4e-main-mode 'emacs)
(evil-set-initial-state 'mu4e-headers-mode 'emacs)
(evil-set-initial-state 'mu4e-view-mode 'emacs)

;; When wrapping mails with w, wrap at 80
(set-fill-column 80)

;; Various keymappings and shortcut functions
(require 'longlines)
(global-set-key [(f12)] 'mu4e)
(global-set-key [(f8)] 'jsrn-mailto-from-kill)
;; Search for the sender of current message
(defun jsrn-search-for-sender ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (from (or (cdr (car (mu4e-message-field msg :from)))
                   (mu4e-warn "No message at point"))))
    (message "%s" from)
    (mu4e~headers-search-execute (concat "contact:" from) t) 
  ))
(fill-keymaps (list mu4e-headers-mode-map
                    mu4e-view-mode-map)
              [(f7)] 'jsrn-search-for-sender
              "w" 'longlines-mode)
(fill-keymap mu4e-headers-mode-map
              (kbd "S-<SPC>") 'scroll-down-command)
(fill-keymap mu4e-view-mode-map
              (kbd "S-<SPC>") 'jsrn-scroll-up
              (kbd "<SPC>") 'jsrn-scroll-down
              (kbd ",") 'jsrn-view-message-expand-links
              (kbd "]") 'mu4e-headers-next-unread
              (kbd "[") '(lambda () (interactive) (mu4e-headers-next-unread t))
              )

;; Workman fixes
(fill-keymap mu4e-headers-mode-map
             evil-down-key   'mu4e-headers-next
             evil-up-key     'mu4e-headers-prev)


;; Setup for making org-capture work with mu4e
;; messages and queries
(require 'org-mu4e)


(message "Loaded mail_setup.el")
(provide 'mail_setup)
