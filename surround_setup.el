;; Emulate surround.vim
;; Usage description really quick:
;; type c s <delimiter> <replacement> for replacing the nearest <delimiter> pair
;; with appropriately chosen <replacement> pair. Use left delimiter to get space
;; on insider, and right delimiter otherwise
;; type d s <delimiter> to remove delimiter pair
;; In visual mode, type s <new delim> to insert delimiter (same rules w. spaces)
;;                 type S <new delim> to insert also newlines on inside

(require 'surround)
(global-surround-mode)
;; Add capability for finding nearest delimiters when typing SPC
;; For surround mode, this is done by redefining two functions sorround-*-overlay
(setq jsrn-delimiter-objects (list "[" "{" "("))
(defun find-nearest-text-objects (&optional types object-map)
  "Find the nearest occurence of a text object like [ and ( using functions
amongst those given in object-map."
  (when (eq nil types)
    (setq types jsrn-delimiter-objects))
  (when (eq nil object-map)
    (setq object-map evil-outer-text-objects-map))
  (let ((tmin -1)
        (tmax most-positive-fixnum))
    (dolist (type types (list tmin tmax))
      (condition-case nil
        (let ((range (funcall (lookup-key object-map type))))
          (when (evil-range-p range)
                (setq tmin (max (evil-range-beginning range) tmin))
                (setq tmax (min (evil-range-end range) tmax)))
          )
        (error nil))
    )))
(defun surround-outer-overlay (char)
  "Return outer overlay for the delimited range represented by CHAR.
This overlay includes the delimiters.
See also `surround-inner-overlay'."
  (let ((range
         (if (string-equal " " (string char))
             ;; choose nearest
             (find-nearest-text-objects)
           ;; we chose a specific delimiter
           (funcall (lookup-key evil-outer-text-objects-map (string char))))))
    (when (evil-range-p range)
      (progn
        (surround-trim-whitespace-from-range range "[ \t]")
        (make-overlay (evil-range-beginning range)
                      (evil-range-end range)
                      nil nil t)))
    ))
(defun surround-inner-overlay (char)
  "Return inner overlay for the delimited range represented by CHAR.
This overlay excludes the delimiters.
See also `surround-outer-overlay'."
  (let ((range
         (if (string-equal " " (string char))
             ;; choose nearest
             (find-nearest-text-objects jsrn-delimiter-objects evil-inner-text-objects-map)
           ;; we chose a specific delimiter
           (funcall (lookup-key evil-inner-text-objects-map (string char))))))
    (when (evil-range-p range)
      (progn
        (when (eq (char-syntax char) ?\()
          (surround-trim-whitespace-from-range range "[ \t]"))
        (make-overlay (evil-range-beginning range)
                      (evil-range-end range)
                      nil nil t))
      )))
;; Add similar functionality for the Evil-born functions c/v + a/i:
(evil-define-text-object jsrn-a-delimiter (count &optional beg end type)
  "select innermost parenthetic delimiter.
note: hackish solution, probably only works for count=1 and more or less none of
the optional values set"
  :extend-selection t
  (find-nearest-text-objects)
  )
(evil-define-text-object jsrn-inside-delimiter (count &optional beg end type)
  "select innermost parenthetic delimiter.
note: hackish solution, probably only works for count=1 and more or less none of
the optional values set"
  :extend-selection nil
  (let ((range (find-nearest-text-objects)))
    (list (+ (evil-range-beginning range) 1) (- (evil-range-end range) 1))
  ))
(fill-keymap evil-visual-state-map
             "a "        'jsrn-a-delimiter
             "i "        'jsrn-inside-delimiter
             )

;; Some extras for certain modes
(add-hook 'LaTeX-mode-hook (lambda ()
                             (push '(?~ . ("\\texttt{" . "}")) surround-pairs-alist)
                             (push '(?/ . ("\\emph{"   . "}")) surround-pairs-alist)
                             (push '(?* . ("\\textbf{" . "}")) surround-pairs-alist)))


;; Replace with clipboard without changing clipboard
(evil-define-operator evil-destroy (beg end type register yank-handler)
  "Destroy text irrevocably"
  (evil-delete beg end type ?_ yank-handler))
(evil-define-operator evil-destroy-replace (beg end type register yank-handler)
  (evil-destroy beg end type register yank-handler)
  (evil-paste-before 1 register))
(define-key evil-motion-state-map (kbd "!") 'evil-destroy-replace)


;; More prominent shortcut for d/v/etc to next close brace/ prev open brace
;; (were "[{" and "]}")
(setq jsrn-delimiter-chars (list ?\[ ?\{ ?\())
(setq jsrn-delimiter-chars-ends (list ?\] ?\} ?\)))
(evil-define-motion beginning-of-delim (count)
  :type exclusive
  (let ((nearest (find-nearest-text-objects jsrn-delimiter-objects evil-inner-text-objects-map)))
    (goto-char (car nearest))
    ))
(evil-define-motion end-of-delim (count)
  :type exclusive
  (let ((nearest (find-nearest-text-objects jsrn-delimiter-objects evil-inner-text-objects-map)))
    (goto-char (car (cdr nearest)))
      ))
(define-key evil-motion-state-map (kbd "p") 'end-of-delim)
(define-key evil-motion-state-map (kbd "P") 'beginning-of-delim)
; reinstate paste in visual mode
(define-key evil-visual-state-map (kbd "p") 'evil-paste-after) 

(message "Loaded surround_setup.el")
(provide 'surround_setup)
