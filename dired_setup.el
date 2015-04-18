;; Dired displays less verbose information
(require 'ls-lisp)
(require 'dired)
(setq ls-lisp-use-insert-directory-program nil)
;; Dired does not open a million buffers
(toggle-diredp-find-file-reuse-dir 1)
(put 'dired-find-alternate-file 'disabled nil)
;; When Dired does something to a file, requiring a target, it suggests other open dired buffer
(setq dired-dwim-target 1)
;; Dired doesn't show dot-files per default. Use C-u s <Ret> to change
(setq dired-listing-switches "-l")

;; redefine this function, to fix the formatting of file sizes in dired mode
(defun ls-lisp-format-file-size (file-size human-readable)
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %11.0f" " %11d") file-size)
    (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
         ;; kilo, mega, giga, tera, peta, exa
         (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
        ((< file-size 1024) (format " %10.0f%s"  file-size (car post-fixes))))))

;; Redefine the sorting in dired to flip between sorting on name, size,
;; time, and extension, rather than simply on name and time.
(defun dired-sort-toggle ()
  ;; Toggle between sort by date/name.  Reverts the buffer.
  (setq dired-actual-switches
        (let (case-fold-search)

          (cond

           ((string-match " " dired-actual-switches) ;; contains a space
            ;; New toggle scheme: add/remove a trailing " -t" " -S",
            ;; or " -U"
            ;; -t = sort by time (date)
            ;; -S = sort by size
            ;; -X = sort by extension

            (cond

             ((string-match " -t\\'" dired-actual-switches)
              (concat
               (substring dired-actual-switches 0 (match-beginning 0))
               " -X"))

             ((string-match " -X\\'" dired-actual-switches)
              (concat
               (substring dired-actual-switches 0 (match-beginning 0))
               " -S"))

             ((string-match " -S\\'" dired-actual-switches)
              (substring dired-actual-switches 0 (match-beginning 0)))

             (t
              (concat dired-actual-switches " -t"))))

           (t
            ;; old toggle scheme: look for a sorting switch, one of [tUXS]
            ;; and switch between them. Assume there is only ONE present.
            (let* ((old-sorting-switch
                    (if (string-match (concat "[t" dired-ls-sorting-switches "]")
                                      dired-actual-switches)
                        (substring dired-actual-switches (match-beginning 0)
                                   (match-end 0))
                      ""))

                   (new-sorting-switch
                    (cond
                     ((string= old-sorting-switch "t")
                      "X")
                     ((string= old-sorting-switch "X")
                      "S")
                     ((string= old-sorting-switch "S")
                      "")
                     (t
                      "t"))))
              (concat
               "-l"
               ;; strip -l and any sorting switches
               (dired-replace-in-string (concat "[-lt"
                                                dired-ls-sorting-switches "]")
                                        ""
                                        dired-actual-switches)
               new-sorting-switch))))))

  (dired-sort-set-modeline)
  (revert-buffer))


;; redefine this fun, to properly provide the modeline in dired mode,
;; supporting the new search modes defined above.
(defun dired-sort-set-modeline ()
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
                   "Dired by date")
                  ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
                   "Dired by ext")
                  ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
                   "Dired by sz")
                  ((string-match "^-[^SXUt]*$" dired-actual-switches)
                   "Dired by name")
                  (t
                   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))

(defun dired-open-native ()
  "Open marked files (or the file the cursor is on) from dired."
  ;; Taken from http://truongtx.me/
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (n (length files)))
    (when (or (<= n 3)
              (y-or-n-p (format "Open %d files?" n)))
      (dolist (file files)
        (call-process "xdg-open"
                      nil 0 nil file)))))
(define-key dired-mode-map (kbd "M-o") 'dired-open-native)

(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)

(defun dired-toggle-hidden-files ()
  "Toggle between showing hidden files or not. This can also be done
using C-u s."
  (interactive)
  (let ((flags dired-actual-switches))
    (progn
      (if (string-match "\\(.*\\)a\\(.*\\)" flags)
          (setq dired-actual-switches
                (concat (match-string 1 flags) (match-string 2 flags)))
        (setq dired-actual-switches (concat flags "a")))
      (revert-buffer)
      (message dired-actual-switches)
      (dired-sort-set-mode-line))
    ))


(fill-keymap dired-mode-map
             "^" 'jsrn-dired-up-directory
             "J" 'dired-goto-file
             "K" 'dired-do-kill-lines
             "e" 'diredp-previous-line
             "r" 'dired-do-redisplay)

(defun jsrn-dired-mode-hook ()
  ;; Highlight current line
  (hl-line-mode)
  (defun jsrn-dired-up-directory ()
    "Go up dir without opening new buffer"
    (interactive)
    (find-alternate-file ".."))
  (fill-keymap evil-normal-state-local-map
               (kbd "SPC") 'jsrn-scroll-down
               (kbd "S-SPC") 'jsrn-scroll-up)
  )
(add-hook 'dired-mode-hook 'jsrn-dired-mode-hook)
(eval-after-load 'dired
  '(fix-evil-workman))
(eval-after-load 'wdired
  '(fix-evil-workman))



(message "Loaded dired_setup.el")
(provide 'dired_setup)
