(defun jsrn-smtpmail-setup ()
  ;; tell message-mode how to send mail
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "web12.meebox.net"
        smtpmail-smtp-service 465
        smtpmail-local-domain "atuin.dk"
        smtpmail-debug-info t ; only to debug problems
        smtpmail-stream-type 'ssl
        smtpmail-queue-dir "~/mail/queued-mail/"
  ))

(defun jsrn-mu4e-setup ()
  (add-to-list 'load-path"/usr/local/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  ;; Handling html messages
  (setq mu4e-html2text-command "html2text -utf8 -width 72")
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
  (setq mu4e-maildir "~/mail"
        mu4e-sent-folder (lambda (msg)
                           (if (eq msg nil)
                               "/atuin/INBOX.Sent"
                             (let ((mailbox (jsrn-mu4e-mailbox msg)))
                               (cond ((string-equal mailbox "atuin") "/atuin/INBOX.Sent")
                                     ((string-equal mailbox "jsrn") "/atuin/INBOX.Sent")
                                     ((string-equal mailbox "dtu")   "/dtu/Sent")))))
        mu4e-drafts-folder "/drafts"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder (lambda (msg)
                             (let ((mailbox (jsrn-mu4e-mailbox msg)))
                               (cond ((string-equal mailbox "atuin") "/atuin/INBOX.Archives.2013")
                                     ((string-equal mailbox "jsrn") "/atuin/INBOX.Archives.2013")
                                     ((string-equal mailbox "dtu")   "/dtu/Archives.2013"))))
        )
  (setq mu4e-attachment-dir "~/downloads")
  ;; Set up some shortcuts access them with 'j' ('jump')
  (setq   mu4e-maildir-shortcuts
          '(("/atuin/INBOX"       . ?i)
            ("/atuin/INBOX.Sent"  . ?s)
            ("/atuin/INBOX.To Use". ?u)
            ("/dtu/INBOX"         . ?I)
            ("/dtu/Sent"          . ?S)
            ("/dtu/To use"        . ?U)
            ))
  ;; Check mail using offlineimap every 5 min
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300
        mail-user-agent 'mu4e-user-agent)
  ;; Messages should be replied from the recipient if it was one of the
  ;; registered email addresses
  (setq mu4e-my-email-addresses '("atuin@atuin.dk"
                                  "jsrn@atuin.dk"
                                  "johan@atuin.dk"
                                  "jsrn@jsrn.dk"
                                  "webmaster@atuin.dk"
                                  "spammy@atuin.dk"
                                  "j.s.r.nielsen@mat.dtu.dk"
                                  "jsrn@dtu.dk"))
  (add-hook 'mu4e-compose-pre-hook
            (defun my-set-from-address ()
              "Set the From address based on the To address of the original."
              (let ((msg mu4e-compose-parent-message))
                (let ((toaddr (cdr (car (mu4e-message-part-field msg :to)))))
                  (message toaddr)
                  (setq user-mail-address
                        (if (member toaddr mu4e-my-email-addresses)
                            toaddr
                          jsrn-user-mail-address))))))
  (setq mu4e-confirm-quit nil)
  (add-hook 'mu4e-compose-mode-hook (defun jsrn-mu4e-compose-setup ()
                                      (flyspell-mode t)))
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
  (define-key mu4e-headers-mode-map (kbd "S-<SPC>") 'scroll-down-command)
  ;; Disable evil
  (evil-set-initial-state 'mu4e-main-mode 'emacs)
  (evil-set-initial-state 'mu4e-headers-mode 'emacs)
  (evil-set-initial-state 'mu4e-view-mode 'emacs)
  ;; Fast opening, since mu4e close all the time
  (global-set-key [(f12)] 'mu4e)
)
