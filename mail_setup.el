(defun jsrn-smtpmail-setup (from-email)
  ;; tell message-mode how to send mail
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-queue-dir "~/mail/queued-mail/"
        smtpmail-debug-info t ; only to debug problems
        )
  (if (eq nil (string-match ".*dtu.dk" from-email))
    (progn
      (setq
        smtpmail-smtp-server "web12.meebox.net"
        smtpmail-smtp-user "atuin@atuin.dk"
        smtpmail-smtp-service 465
        smtpmail-local-domain "atuin.dk"
        smtpmail-stream-type 'ssl
        )
     (message "Using Meebox SMTP"))
    (progn
        (setq
        smtpmail-smtp-server "smtpauth.imm.dtu.dk"
        smtpmail-smtp-user "jsrn"
        smtpmail-smtp-service 465
        smtpmail-local-domain nil
        smtpmail-stream-type 'ssl
        )
       (message "Using DTU SMTP"))
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

(defun jsrn-mailto-from-kill ()
  "Parse the contents of the top of the kill ring as a mailto-link and apply it"
  (interactive)
  (setq uri (current-kill 0 t))
  (set-text-properties 0 (length uri) nil uri)
  (when (string-match "\".*\"" uri)
    (setq uri (substring uri 1 (- (length uri) 1))))
  (mu4e~compose-browse-url-mail uri))

(defun jsrn-mu4e-setup ()
  (add-to-list 'load-path"/usr/local/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  
  ;; Handling html messages
  (setq mu4e-html2text-command "vilistextum -k -y\"iso-8859-1\" - -")
  (setq mu4e-view-prefer-html t)
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
        jsrn-mu4e-sent-folder "/atuin/INBOX.Sent"
        mu4e-sent-folder (lambda (msg)
                           (if (eq msg nil)
                               (progn
                                 (message "Sent mail copied to default sent folder: %s" jsrn-mu4e-sent-folder)
                               jsrn-mu4e-sent-folder)
                             (let ((mailbox (jsrn-mu4e-mailbox msg)))
                               (cond ((string-equal mailbox "atuin") "/atuin/INBOX.Sent")
                                     ((string-equal mailbox "jsrn") "/atuin/INBOX.Sent")
                                     ((string-equal mailbox "dtu")   "/dtu/Sent")
                                     ((string-equal mailbox "gmail")   "/atuin/INBOX.Sent")))))
        mu4e-drafts-folder "/drafts"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder (lambda (msg)
                             (let ((mailbox (jsrn-mu4e-mailbox msg)))
                               (cond ((string-equal mailbox "atuin") "/atuin/INBOX.Archives.2013")
                                     ((string-equal mailbox "jsrn") "/atuin/INBOX.Archives.2013")
                                     ((string-equal mailbox "gmail") "/atuin/INBOX.Archives.2013")
                                     ((string-equal mailbox "dtu")   "/dtu/Archives.2013"))))
        )
  (setq mu4e-attachment-dir "~/downloads")

  ;; Set up some shortcuts access them with 'j' ('jump')
  (setq   mu4e-maildir-shortcuts
          '(("/atuin/INBOX"       . ?i)
            ("/atuin/INBOX.Sent"  . ?s)
            ("/atuin/INBOX.To Use". ?u)
            ("/atuin/INBOX.Archives.2013". ?a)
            ("/dtu/INBOX"         . ?I)
            ("/dtu/Sent"          . ?S)
            ("/dtu/To use"        . ?U)
            ("/atuin/INBOX.To Use". ?u)
            ("/dtu/Archives.2013" . ?A)
            ))
  
  ;; View the contents of all inboxes with 'bi'
  (add-to-list 'mu4e-bookmarks
     '("maildir:/atuin/INBOX or maildir:/dtu/INBOX"  "Inboxes"  ?i))

  ;; Check mail using offlineimap every 5 min
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300
        mail-user-agent 'mu4e-user-agent)

  ;; Messages should be replied from the recipient if it was one of the
  ;; registered email addresses
  (setq mu4e-my-email-addresses '("atuin@atuin.dk"
                                  "jsrn@jsrn.dk"
                                  "johan.nielsen@uni-ulm.de"
                                  "jsrn@dtu.dk"
                                  "spammy@atuin.dk"
                                  "jsrn@atuin.dk"
                                  "johan@atuin.dk"
                                  "webmaster@atuin.dk"
                                  "j.s.r.nielsen@mat.dtu.dk"
                                  "santaphile@gmail.dk"))
  (setq jsrn-mu4e-email-to-sent-folder '(("j.s.r.nielsen@mat.dtu.dk" . "/dtu/Sent")
                                         ("jsrn@dtu.dk" . "/dtu/Sent"))
        jsrn-mu4e-mailbox-default "/atuin/INBOX.Sent")
  
  ;; Only addresses from mail sent to me directly should go in auto-completions
  (setq mu4e-compose-complete-only-personal nil)

  ;; Switch my from address to the next possible from address
  (defun next-from-address ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward "<\\(.*\\)>")
      (let* ((oldmail (match-string 1))
            (inlist (member oldmail mu4e-my-email-addresses))
            (email  (if (and inlist (cdr inlist))
                        (car (cdr inlist))
                      (car mu4e-my-email-addresses))))
        (message "%s" inlist)
        (replace-match email nil nil nil 1))
    )
  )
  (define-key mu4e-compose-mode-map [(f2)] 'next-from-address)

  (defun jsrn-set-from-address ()
    "Set the From address, and Sent Folder based on the To address of the original"
    (setq user-mail-address
          (let ((msg mu4e-compose-parent-message))
            (if msg
                (let ((toaddr (cdr (car (mu4e-message-part-field msg :to)))))
                  (if (and (not (eq toaddr nil)) (member (downcase toaddr) mu4e-my-email-addresses))
                      (downcase toaddr)
                    jsrn-user-mail-address))
              jsrn-user-mail-address)))
    (let ((folder (cdr (assoc user-mail-address jsrn-mu4e-email-to-sent-folder))))
      (if (eq folder nil)
          (setq jsrn-mu4e-sent-folder jsrn-mu4e-mailbox-default)
        (setq jsrn-mu4e-sent-folder folder)))
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
                  )
    )
  (add-hook 'mu4e-compose-mode-hook 'jsrn-mu4e-compose-setup)

  ;; Don't ask on exiting
  (setq mu4e-confirm-quit nil)

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

  ;; Various keymappings and shortcut functions
  (global-set-key [(f12)] 'mu4e)
  (global-set-key [(f8)] 'jsrn-mailto-from-kill)
  ;; Search for the sender of current message
  (defun jsrn-search-for-sender ()
    (interactive)
    (let* ((msg (mu4e-message-at-point))
           (from (or (cdr (car (mu4e-message-field msg :from)))
                     (mu4e-warn "No message at point"))))
      (message "%s" from)
      (mu4e~headers-search-execute (concat "from:" from) t) 
    ))
  (fill-keymaps (list mu4e-headers-mode-map
                      mu4e-view-mode-map)
                [(f7)] 'jsrn-search-for-sender)
  (fill-keymap mu4e-headers-mode-map
                (kbd "S-<SPC>") 'scroll-down-command)
  (fill-keymap mu4e-view-mode-map
                (kbd "S-<SPC>") 'jsrn-scroll-up
                (kbd "<SPC>") 'jsrn-scroll-down
                )
)
