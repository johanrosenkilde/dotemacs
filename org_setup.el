;; My setup for Org mode and Agenda mode
(setq org-startup-indented t
      org-deadline-warning-days 7
      )
(defun jsrn-agenda-activate ()
  "Activate the current Emacs as an agenda Emacs. Weird stuff seem to happen
sometimes if more than one Emacs has this set"
  (interactive)
  (find-file "~/orgs/home.org")
  ;; Set files which contains agenda files to all .org files in specified dir
  (setq org-agenda-files (directory-files "~/orgs" t "^[^#]*org$" t))
  ;; Various agenda setup
  (setq org-agenda-repeating-timestamp-show-all nil) ; don't show repititions in agenda
  ;; Setup org-capture
  (setq org-default-notes-file "~/orgs/home.org")
  (defun jsrn-read-date-prob-two-weeks ()
      (concat "<" (org-read-date nil nil nil nil nil "+14") ">"))
  (setq org-capture-templates
        (list
         '("a" "Do (no capture)" entry (file+headline "~/orgs/home.org" "Reminders")
           "* TODO Do %^{Description}\n%^t\n%U\n\n")
         '("r" "respond" entry (file+headline "~/orgs/home.org" "Reminders")
           "* TODO Besvar %:from on %:subject\nSCHEDULED: %(jsrn-read-date-prob-two-weeks)\n%U\n%a\n\n")
         '("t" "ticket" entry (file+headline "~/orgs/home.org" "Reminders")
          "* Ticket for %:subject (%:from) \n%^t\n%U\n%a\n\n")
         '("h" "handle" entry (file+headline "~/orgs/home.org" "Reminders")
          "* Handle %:subject from %:from \nSCHEDULED: %(jsrn-read-date-prob-two-weeks)\n%U\n%a\n\n")
         '("e" "event" entry (file+headline "~/orgs/home.org" "Reminders")
          "* Event %:subject from %:from \n%^t\n%U\n%a\n\n")
        ))
  (define-key global-map "\C-cc" 'org-capture)
  ;; Reminder support for Org
  (defun jsrn-org-agenda-to-appt ()
    "Erase all reminders and rebuilt reminders for today from the agenda"
    (interactive)
    (org-agenda-to-appt 'refresh)
    )
  ;; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-finalize-agenda-hook 'jsrn-org-agenda-to-appt 'append)
  ;; Rebuild agenda reminders
  (jsrn-org-agenda-to-appt)
  ;; Activate appointments so we get notifications
  (appt-activate t)
  (setq appt-display-format 'window)
  (defun appt-disp-window (mins curtime text)
    "Redefine Appointment reminder function to show a Memo using system call"
    (call-process "/usr/bin/notify-send" nil nil nil (format "Appointment:\n%s \n in  %s min" text mins)))
  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" nil 'jsrn-org-agenda-to-appt)
  (fill-keymap org-agenda-mode-map
               evil-up-key     'org-agenda-previous-line
               evil-down-key   'org-agenda-next-line
               )
  )
(add-hook 'administrative-mode-hook 'jsrn-agenda-activate)

(defun jsrn-administrative-org-mode-hook ()
  (defun is-org (buf)
    "Return whether the given buffer has an open org file or not"
    (let ((filename (buffer-file-name buf)))
      (and filename (string-match "\\.org$" filename))))
  (defun jsrn-show-last-org-buffer ()
    "Goto the last visited org buffer"
    (interactive)
    (let ((bufs (buffer-list)))
      (while (not (is-org (car bufs)))
        (setq bufs (cdr bufs)))
      (set-window-buffer nil (car bufs))))
  (defun cycle-agenda-files-or-goto-org ()
    (interactive)
    (if (is-org (current-buffer))
        (org-cycle-agenda-files)
      (jsrn-show-last-org-buffer)))
  (global-set-key (kbd "C-,") 'cycle-agenda-files-or-goto-org)
)
(add-hook 'administrative-mode-hook 'jsrn-administrative-org-mode-hook)

(defun jsrn-org-mode-hook ()
  (visual-line-mode t)
  (evil-declare-motion 'org-up-element)
  (fill-keymaps (list org-mode-map)
                (kbd (concat "M-" evil-left-key))  'org-metaleft
                (kbd (concat "M-" evil-down-key))  'org-metadown
                (kbd (concat "M-" evil-up-key))    'org-metaup
                (kbd (concat "M-" evil-right-key)) 'org-metaright)
  (fill-keymap org-mode-map
               (kbd (concat "M-" evil-left-key-uc))  'org-shiftmetaleft
               (kbd (concat "M-" evil-down-key-uc))  'org-shiftmetadown
               (kbd (concat "M-" evil-up-key-uc))    'org-shiftmetaup
               (kbd (concat "M-" evil-right-key-uc)) 'org-shiftmetaright
               (kbd (concat "C-M-" evil-up-key))    'org-backward-element
               (kbd (concat "C-M-" evil-down-key)) 'org-forward-element
               (kbd "C-c l") 'org-store-link ;; insert it with C-c C-l (org-insert-link)
               (kbd "C-c a") 'org-agenda)
  ;; to override evil binding for ~, we do it on the evil local maps
  (fill-keymaps (list evil-motion-state-local-map
                      evil-visual-state-local-map
                      evil-normal-state-local-map)
                (kbd "~")  (lambda () (interactive) (progn
                                                      (evil-set-jump)
                                                      (org-up-element))))
  ;; Let org mode override M-n
  (define-key evil-normal-state-local-map (kbd "M-n") 'org-metadown)
  ;; Let winner keys overwrite org-mode
  (define-key evil-normal-state-local-map (kbd "M-S-<left>") 'winner-undo) 
  (define-key evil-normal-state-local-map (kbd "M-S-<right>") 'winner-redo)

  ;; Org Clock report by days
  (defun org-dblock-write:rangereport (params)
    "Display day-by-day time reports."
    (let* ((ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (start (time-to-seconds
                   (apply 'encode-time (org-parse-time-string ts))))
           (end (time-to-seconds
                 (apply 'encode-time (org-parse-time-string te))))
           (curday end)
           day-numbers)
      (setq params (plist-put params :tstart nil))
      (setq params (plist-put params :end nil))
      (while (>= curday start)
        (save-excursion
          (insert "\n\n"
                  (format-time-string "%A, %e. of %B"
                                      (seconds-to-time curday))
                  "\n")
          (org-dblock-write:clocktable
           (plist-put
            (plist-put
             params
             :tstart
             (format-time-string (car org-time-stamp-formats)
                                 (seconds-to-time curday)))
            :tend
            (format-time-string (car org-time-stamp-formats)
                                (seconds-to-time (+ curday 86400)))))
          (setq curday (- curday 86400))))))

  (jsrn-org-clock-mode-overrides) ;see below
)

(add-hook 'org-mode-hook 'jsrn-org-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Override a few Org functions so that org-clock-report includes also headings
;; with no time in Search for JSRN to find the diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "org-clock"
  '(progn
                                        ; (defadvice org-clock-sum (around jsrn-org-clock-sum (&optional tstart tend headline-filter propname))
     (defun org-clock-sum (&optional tstart tend headline-filter propname)
       "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.
HEADLINE-FILTER is a zero-arg function that, if specified, is called for
each headline in the time range with point at the headline.  Headlines for
which HEADLINE-FILTER returns nil are excluded from the clock summation.
PROPNAME lets you set a custom text property instead of :org-clock-minutes."
       (interactive)
       (org-with-silent-modifications
        (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
                           org-clock-string
                           "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
               (lmax 30)
               (ltimes (make-vector lmax 0))
               (t1 0)
               (level 0)
               ts te dt
               time)
          (if (stringp tstart) (setq tstart (org-time-string-to-seconds tstart)))
          (if (stringp tend) (setq tend (org-time-string-to-seconds tend)))
          (if (consp tstart) (setq tstart (org-float-time tstart)))
          (if (consp tend) (setq tend (org-float-time tend)))
          (remove-text-properties (point-min) (point-max)
                                  `(,(or propname :org-clock-minutes) t
                                    :org-clock-force-headline-inclusion t))
          (save-excursion
            (goto-char (point-max))
            (while (re-search-backward re nil t)
              (cond
               ((match-end 2)
                ;; Two time stamps
                (setq ts (match-string 2)
                      te (match-string 3)
                      ts (org-float-time
                          (apply 'encode-time (org-parse-time-string ts)))
                      te (org-float-time
                          (apply 'encode-time (org-parse-time-string te)))
                      ts (if tstart (max ts tstart) ts)
                      te (if tend (min te tend) te)
                      dt (- te ts)
                      t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1)))
               ((match-end 4)
                ;; A naked time
                (setq t1 (+ t1 (string-to-number (match-string 5))
                            (* 60 (string-to-number (match-string 4))))))
               (t ;; A headline
                ;; Add the currently clocking item time to the total
                (when (and org-clock-report-include-clocking-task
                           (equal (org-clocking-buffer) (current-buffer))
                           (equal (marker-position org-clock-hd-marker) (point))
                           tstart
                           tend
                           (>= (org-float-time org-clock-start-time) tstart)
                           (<= (org-float-time org-clock-start-time) tend))
                  (let ((time (floor (- (org-float-time)
                                        (org-float-time org-clock-start-time)) 60)))
                    (setq t1 (+ t1 time))))
                (let* ((headline-forced
                        (get-text-property (point)
                                           :org-clock-force-headline-inclusion))
                       (headline-included
                        (or (null headline-filter)
                            (save-excursion
                              (save-match-data (funcall headline-filter))))))
                  (setq level (- (match-end 1) (match-beginning 1)))
                  ;; JSRN: The following line was previously
                                        ; (when (or (> t1 0) (> (aref ltimes level) 0))
                  (when (or (>= t1 0) (> (aref ltimes level) 0))
                    (when (or headline-included headline-forced)
                      (if headline-included
                          (loop for l from 0 to level do
                                (aset ltimes l (+ (aref ltimes l) t1))))
                      (setq time (aref ltimes level))
                      (goto-char (match-beginning 0))
                      (put-text-property (point) (point-at-eol)
                                         (or propname :org-clock-minutes) time)
                      (if headline-filter
                          (save-excursion
                            (save-match-data
                              (while
                                  (> (funcall outline-level) 1)
                                (outline-up-heading 1 t)
                                (put-text-property
                                 (point) (point-at-eol)
                                 :org-clock-force-headline-inclusion t))))))
                    (setq t1 0)
                    (loop for l from level to (1- lmax) do
                          (aset ltimes l 0)))))))
            (setq org-clock-file-total-minutes (aref ltimes 0))))))

                                        ; (defadvice org-clock-get-table-data (around jsrn-org-clock-get-table-data (file params))
     (defun org-clock-get-table-data (file params)
       "Get the clocktable data for file FILE, with parameters PARAMS.
FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TIMESTAMP TIME)

LEVEL:     The level of the headline, as an integer.  This will be
           the reduced leve, so 1,2,3,... even if only odd levels
           are being used.
HEADLINE:  The text of the headline.  Depending on PARAMS, this may
           already be formatted like a link.
TIMESTAMP: If PARAMS require it, this will be a time stamp found in the
           entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
           in this sequence.
TIME:      The sum of all time spend in this tree, in minutes.  This time
           will of cause be restricted to the time block and tags match
           specified in PARAMS."
       (let* ((maxlevel (or (plist-get params :maxlevel) 3))
              (timestamp (plist-get params :timestamp))
              (ts (plist-get params :tstart))
              (te (plist-get params :tend))
              (ws (plist-get params :wstart))
              (ms (plist-get params :mstart))
              (block (plist-get params :block))
              (link (plist-get params :link))
              (tags (plist-get params :tags))
              (properties (plist-get params :properties))
              (inherit-property-p (plist-get params :inherit-props))
              todo-only
              (matcher (if tags (cdr (org-make-tags-matcher tags))))
              cc range-text st p time level hdl props tsp tbl)

         (setq org-clock-file-total-minutes nil)
         (when block
           (setq cc (org-clock-special-range block nil t ws ms)
                 ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
         (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
         (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
         (when (and ts (listp ts))
           (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
         (when (and te (listp te))
           (setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
         ;; Now the times are strings we can parse.
         (if ts (setq ts (org-float-time
                          (seconds-to-time (org-matcher-time ts)))))
         (if te (setq te (org-float-time
                          (seconds-to-time (org-matcher-time te)))))
         (save-excursion
           (org-clock-sum ts te
                          (unless (null matcher)
                            (lambda ()
                              (let* ((tags-list (org-get-tags-at))
                                     (org-scanner-tags tags-list)
                                     (org-trust-scanner-tags t))
                                (eval matcher)))))
           (goto-char (point-min))
           (setq st t)
           (while (or (and (bobp) (prog1 st (setq st nil))
                           (get-text-property (point) :org-clock-minutes)
                           (setq p (point-min)))
                      (setq p (next-single-property-change
                               (point) :org-clock-minutes)))
             (goto-char p)
             (when (setq time (get-text-property p :org-clock-minutes))
               (save-excursion
                 (beginning-of-line 1)
                 (when (and (looking-at (org-re "\\(\\*+\\)[ \t]+\\(.*?\\)\\([ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
                            (setq level (org-reduced-level
                                         (- (match-end 1) (match-beginning 1))))
                            (<= level maxlevel))
                   (setq hdl (if (not link)
                                 (match-string 2)
                               (org-make-link-string
                                (format "file:%s::%s"
                                        (buffer-file-name)
                                        (save-match-data
                                          (match-string 2)))
                                (org-make-org-heading-search-string
                                 (replace-regexp-in-string
                                  org-bracket-link-regexp
                                  (lambda (m) (or (match-string 3 m)
                                                  (match-string 1 m)))
                                  (match-string 2)))))
                         tsp (when timestamp
                               (setq props (org-entry-properties (point)))
                               (or (cdr (assoc "SCHEDULED" props))
                                   (cdr (assoc "DEADLINE" props))
                                   (cdr (assoc "TIMESTAMP" props))
                                   (cdr (assoc "TIMESTAMP_IA" props))))
                         props (when properties
                                 (remove nil
                                         (mapcar
                                          (lambda (p)
                                            (when (org-entry-get (point) p inherit-property-p)
                                              (cons p (org-entry-get (point) p inherit-property-p))))
                                          properties))))
                                        ; JSRN: The following line used to be
                                        ;(when (> time 0) (push (list level hdl tsp time props) tbl))))))
                   (push (list level hdl tsp time props) tbl)))))
           (setq tbl (nreverse tbl))
           (list file org-clock-file-total-minutes tbl))))
     
     ))