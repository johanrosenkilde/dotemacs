;; A MOTLEY OF MY OWN HELPER FUNCTIONS

(defun jsrn-recompile ()
  (interactive)
  (progn
    (save-buffer)
    (if (fboundp 'recompile)
        (progn
          ;; This code is complicated by latex compilation
          ;; not responding to SIGINT; otherwise, we
          ;; could've just used kill-compilation
          (ignore-errors
            (process-kill-without-query
             (get-buffer-process
              (get-buffer "*compilation*"))))
          (ignore-errors
            (kill-buffer "*compilation*"))
          (recompile))
      (compile)
    )))

(defun kill-line-backwards ()
  "Kill the current line backwards from the current column.

Kill the current line backwards from the current column. If at col 0, kill
only the newline character"
  (interactive)
  (if (= (current-column) 0) ; If we are at beginning, kill newline char
      (backward-delete-char 1)
    (kill-line 0)))

(defun beginning-of-visual-line-smart ()
  "Move point to first non-whitespace character or beginning-of-visual-line.

Move point to the first non-whitespace character on this visual line.
If point was already at that position, move point to beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    ;; the following is a paraphrasing of back-to-indentation, but with visual-line
    (beginning-of-visual-line 1)
    (skip-syntax-forward " " (line-end-position))
    (backward-prefix-chars)
    ;; if we didn't move, move instead before the indent.
    (and (= oldpos (point))
         (beginning-of-line))))

(defun mark-current-line-smart ()
  "Smartly mark the current line in Evil char mode, i.e. without leading space
and trailing. Assumes one is in visual mode\n"
  (interactive)
  (beginning-of-line 1)
  (skip-syntax-forward " " (line-end-position))
  (exchange-point-and-mark)
  (end-of-line)
  (evil-backward-char) ; corner-case: normal mode with cursor at eol
  )

(defun search-region (forward)
  "Search for the text in the region. Search forward iff FORWARD is `t`"
  (interactive)
  (let ((text (buffer-substring evil-visual-beginning evil-visual-end))
        (begin (if forward (+ (point) 1) (- (point) 1))))
    (evil-push-search-history text forward)
    (evil-search text forward nil begin)
    (evil-visual-select (point) (+ (point) (- (length text) 1)))
    )
  )

(defun search-region-backward ()
  "Search backward for the text in the region"
  (interactive)
  (search-region nil)
  )
(defun search-region-forward ()
  "Search forward for the text in the region"
  (interactive)
  (search-region t)
  )

;;TODO: delete-visual-line to replace S-d

;;Function for reloading the .emacs file
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))

(defun kill-process-interactive ()
  (interactive)
  (let ((pname (ido-completing-read "Process Name: " 
                    (mapcar 'process-name (process-list)))))

    (delete-process (get-process pname))))

(defun stop-process-interactive ()
  (interactive)
  (let ((pname (ido-completing-read "Process Name: "
                    (mapcar 'process-name (process-list)))))

    (stop-process (get-process pname))))

;; Block movement
(setq block-delimiter "[:blank:]*$")
(defun backward-block ()
  "Move backwards to the last beginning of a block."
  (interactive)
  (backward-char 1)
  (search-backward-regexp (concat "^" block-delimiter) nil 0))

(defun forward-block ()
  "Move forwards to the next beginning of a block."
  (interactive)
  ; If point is on a delimiter, we should skip this, so search from beginning of
  ; next line (this will match immediately, if next line is a delimiter)
  (forward-line)
  ; search forward: if it worked, move to begin of delimiter, otherwise end of file
  (when (search-forward-regexp (concat "^" block-delimiter) nil 0)
      (goto-char (match-beginning 0))))

(defun yank-block ()
  "Yank the block point is currently in"
  (interactive)
  (save-excursion
    (let ((begin
          (progn
            (unless (looking-at (concat "^" block-delimiter))
              (backward-block))
            (point))))
      (forward-block)
      (evil-yank-lines begin (point))
    ))
  )

(defun toggle-fullscreen ()
  "Toggle full screen on X11.
  By Ivan Kanis: Harvested from EmacsWiki 2013-12-20"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun set-term-frame ()
  (interactive)
  (set-background-color "black")
  (set-foreground-color "grey")
  (set-frame-width (selected-frame) 100)
  (get-term)
  )

(defun show-clock ()
  (interactive)
  (pos-tip-show (concat " It is now:\n " (current-time-string)) )
  )

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

Prefixed with \\[universal-argument], expand the file name to
its fully canocalized path.  See `expand-file-name'.

Prefixed with \\[negative-argument], use relative path to file
name from current directory, `default-directory'.  See
`file-relative-name'.

The default with no prefix is to insert the file name exactly as
it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

;; Create a list of all functions that can be called (interactive or non)
(setq list-of-all-functions
      (progn 
        (setq l nil)
        (mapatoms 
         (lambda (x)
           (and (fboundp x)                          ; does x name a function?
                (add-to-list 'l (symbol-name x)))))
        (sort l 'string<)
        ))

(defun call-function (fun &optional args)
  "Call the named function without arguments and put the results in a temporary buffer"
  (interactive
   ;; (list (read-string "Enter function name: ")))
   (list (let ((smex-prompt-string "Enter function name: "))
            (smex-completing-read list-of-all-functions nil))))
  (with-output-to-temp-buffer (concat "Output of " fun)
      (princ (format "%s" (funcall (intern fun))))))

(defun name-of-keymap (keymap)
  "Return the symbol (i.e. name) to which KEYMAP is bound, or nil if no such symbol exists.
Use for e.g. (keymap-symbol (current-local-map))."
  ;; By StackOverflow user4815162342
  ;; From https://stackoverflow.com/questions/14489848/emacs-name-of-current-local-keymap.
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

(defun describe-key-all (key)
  "Print all functions and their key-maps in order of search which defines the
  key binding."
  (interactive "kDescribe key (or click or menu item): ")
  ;;TODO: Should search through all the keymaps defined, e.g.
  ;;emulation-mode-map-alists (a list of list of maps, includes evil-modes)
  ;;See: http://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html#Searching-Keymaps
  (let ((local-key (local-key-binding key))
        (global-key (global-key-binding key))
        (minors (progn
                  (setq res nil)
                  (dolist (mmap minor-mode-map-alist res) 
                    (let* ((mapname (car mmap))
                           (map (cdr mmap))
                           (lookup (lookup-key map key)))
                      (when lookup (add-to-list 'res (cons mapname lookup))))
                    )))
        (active (current-active-maps)))
    (with-output-to-temp-buffer "*Describe all bindings to key*"
      (princ (format "ALL loaded key maps which define the key binding\n\t%s\n\n\n" (key-description key)))
      (when local-key
        (princ (format "Local key map (%s):\t\t`%s'\t\t*DEFINED*\n\n" (name-of-keymap (current-local-map)) local-key))
        )
      (when minors
        (progn
          (princ "Minor key maps:\n")
          (dolist (minor minors)
            (princ (format "\t%s\t`%s'\t%s\n" (car minor) (cdr minor) 
                           (if (-find-index (lambda (active) (eq active (car minor))) active) "*DEFINED*" ""))))
          (princ "\n")
          )
        )
      (when global-key
        (princ (format "Global key map:\t\t`%s'\t\t*DEFINED*\n\n" global-key))
        )
      )
    )) 

(defun modify-font-height (modifier)
  "Modify the font size by amount modifier"
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute modifier))))
