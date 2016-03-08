;; Activate the current Emacs as an agenda Emacs. Weird stuff seem to happen
;; sometimes if more than one Emacs has this set

;; Open one of the org files
(find-file "~/orgs/home.org")

;; Set files which contains agenda files to all .org files in specified dir
(setq org-agenda-files (directory-files "~/orgs" t "^[^#]*org$" t))

;; Various agenda setup
(setq org-todo-keywords '((sequence "TODO" "DONE")))
(setq org-agenda-repeating-timestamp-show-all (list "MEET")) ; only show repetitions for MEET in agenda



;; Setup org-capture
(setq org-default-notes-file "~/orgs/home.org")

(defun jsrn-read-date-prob-two-weeks ()
  "Helper for org-capture for asking for a date, suggesting 14 days in the future"
    (concat "<" (org-read-date nil nil nil nil nil "+14") ">"))

(setq org-capture-templates
      (list
       '("a" "No capture" entry (file+headline "~/orgs/home.org" "Reminders")
         "* %^{Description}\n%^t\n%U\n\n")
       '("A" "No capture (work)" entry (file+headline "~/orgs/work.org" "Work Reminders")
         "* %^{Description}\n%^t\n%U\n\n")
       '("r" "respond" entry (file+headline "~/orgs/home.org" "Reminders")
         "* Besvar %:from on %:subject\nSCHEDULED: %(jsrn-read-date-prob-two-weeks)\n%U\n%a\n\n")
       '("R" "respond (w0rk)" entry (file+headline "~/orgs/work.org" "Work Reminders")
         "* Besvar %:from on %:subject\nSCHEDULED: %(jsrn-read-date-prob-two-weeks)\n%U\n%a\n\n")
       '("t" "ticket" entry (file+headline "~/orgs/home.org" "Reminders")
        "* Ticket for %:subject (%:from) \n%^t\n%U\n%a\n\n")
       '("h" "handle" entry (file+headline "~/orgs/home.org" "Reminders")
        "* Handle %:subject from %:from \nDEADLINE: %(jsrn-read-date-prob-two-weeks)\n%U\n%a\n\n")
       '("H" "handle (work)" entry (file+headline "~/orgs/work.org" "Work Reminders")
        "* Handle %:subject from %:from \nDEADLINE: %(jsrn-read-date-prob-two-weeks)\n%U\n%a\n\n")
       '("e" "event" entry (file+headline "~/orgs/home.org" "Reminders")
        "* Event %:subject from %:from \n%^t\n%U\n MONKEY %a MONKEY\n\n")
       '("E" "event (work)" entry (file+headline "~/orgs/work.org" "Work Reminders")
        "* Event %:subject from %:from \n%^t\n%U\n%a\n\n")
      ))

(setq org-finalize-agenda-hook ())
;; Colourise appointments by file
(add-hook 'org-finalize-agenda-hook
    (lambda ()
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "work:" nil t) 
           (add-text-properties (match-beginning 0) (point-at-eol)
                                '(face (:background "#cbf2bd")))))))



;; Appointment notifications
(defun jsrn-org-agenda-to-appt ()
  "Erase all reminders and rebuilt reminders for today from the agenda"
  (interactive)
  (org-agenda-to-appt 'refresh)
  )

;; Rebuild the notficiation-database everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'jsrn-org-agenda-to-appt 'append)

;; Rebuild notification-database now
(jsrn-org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; Setup how notifications look
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


;; Org helper functions
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
  "Cycle through Org files"
  (interactive)
  (if (is-org (current-buffer))
      (org-cycle-agenda-files)
    (jsrn-show-last-org-buffer)))


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

(defun org-archive-handled-subtrees ()
  "Regarding subtrees as timed appointments/deadlines/schedules/etc., archive
  everything which is closed."
  (interactive)
  (let ((f (lambda ()
             (let* ((element (org-element-at-point))
                    (headline (org-element-property :title element))
                    (get-time-of-named (lambda (named)
                                         (let ((breakdown (car (cdr named))))
                                           (org-time-string-to-seconds (plist-get breakdown :raw-value)))))
                    (timestamp (if (org-element-property :scheduled element) ;; It's a scheduled item
                                   (funcall get-time-of-named (org-element-property :scheduled element))
                                 (if (org-element-property :deadline element) ;; It's a deadline item
                                     (funcall get-time-of-named (org-element-property :deadline element))
                                   ;; It's something else, see if it has a
                                   ;; timestamp (look for second one in a
                                   ;; possible date range by appending [^-])
                                   (progn
                                     (when (re-search-forward (concat (org-re-timestamp 'active) "\\($\\|[^-]\\)")
                                                              (org-element-property :end element) t)
                                       (let ((timestamp (match-string 1)))
                                         ;; Check that we didn't go to a sub-element
                                         (when (eq (org-element-property :contents-begin element)
                                                   (org-element-property :begin (org-element-at-point)))
                                           (org-time-string-to-seconds timestamp)))
                                       ))
                                   )))
                    (do-archive (and ;; Ask for archiving when
                                     ;; There is a timestamp
                                     timestamp
                                     ;; The timestamp is passed
                                     (< timestamp (time-to-seconds (current-time)))
                                     ;; The element is not marked TODO
                                     (not (equal "TODO" (org-element-property :todo-keyword element)))
                                     ;; The element is a still-open scheduled element
                                     (or (not (org-element-property :scheduled element))
                                         (equal "DONE" (org-element-property :todo-keyword element))
                                         )
                                     ))
                    )
               (when (and do-archive
                          (y-or-n-p (format "Move subtree '%s' to archive? " headline)))
                 (org-archive-subtree-default))
               )))
        )
    (org-map-entries f nil 'tree))
  )


;; Keybindings
(fill-keymap org-mode-map
             (kbd "C-c a") 'org-agenda
             )
(global-set-key [(f10)] 'org-agenda-list)
(global-set-key [(shift f10)] '(lambda ()
  (interactive)
  (org-agenda-list nil
                   (concat (format-time-string "%m") "-01")
                   'month)))
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-,") 'cycle-agenda-files-or-goto-org)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Override a few Org functions so that org-clock-report includes also headings
;; with no time in.
;; Search for JSRN to find the diff
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




(message "Loaded administrative_setup.el")
(provide 'administrative_setup)
