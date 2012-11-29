
(set-face-attribute 'default nil :height 70)
(setq inhibit-splash-screen t)
(tool-bar-mode 0)

; Other global nice options
(toggle-scroll-bar -1) ;; Emacs gurus don't need no stinking scroll bars
(set-fringe-mode '(0 . 1)) ;activate only the right fringe area
(setq compilation-scroll-output t)

; File type default modes
(add-to-list 'auto-mode-alist '("\\svg\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\env\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\scene\\'" . xml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       GLOBALLY DEFINED CUSTOM FUNCTIONS AND KEYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [(f2)] '(lambda () (interactive) (save-buffer) (recompile)))
(global-set-key "\M-?" 'hippie-expand)

(defun kill-line-backwards ()
  "Kill the current line backwards from the current column.

Kill the current line backwards from the current column. If at col 0, kill
only the newline character"
  (interactive)
  (if (= (current-column) 0) ; If we are at beginning, kill newline char
    (backward-delete-char 1)
    (kill-line 0)))
(global-set-key (kbd "M-C-<backspace>") 'kill-line-backwards)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-beginning-of-line) ;Override default C-a

;Function for reloading the .emacs file
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PACKAGE-INSTALL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Other packages
; Fill sentence: reflows paragraph to have only linebreaks at sentence boundaries
(load "~/.emacs.d/fill-sentence.el")

; minor mode Highlight parentheses which are around cursor
(require 'highlight-parentheses)

;; Global loading
; Load ido -- alternatives shown directly in minibuffer + more
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;match substr on what is written

; Uniquify gives better names to buffers containing files with same base name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

; Winner gives undo and redo of windows arrangements
(require 'winner)
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       EVIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(setq evil-find-skip-newlines t)
(setq evil-ex-search-highlight-all nil)
(setq evil-want-fine-undo t)
(setq evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))
;; Escape quits anything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;; Control-e and Control-a works everywhere
(define-key evil-normal-state-map (kbd "C-e") 'end-of-visual-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-visual-line)
(define-key evil-motion-state-map (kbd "C-e") 'end-of-visual-line)
(define-key evil-normal-state-map (kbd "C-a") 'smart-beginning-of-visual-line)
(define-key evil-insert-state-map (kbd "C-a") 'smart-beginning-of-visual-line)
(define-key evil-motion-state-map (kbd "C-a") 'smart-beginning-of-visual-line)
;; Search using Emacs' isearch but using Vim keybindings
(define-key evil-normal-state-map "/" 'isearch-forward)
(define-key evil-normal-state-map "?" 'isearch-backward)
(define-key evil-normal-state-map "n" 'isearch-repeat-forward)
(define-key evil-normal-state-map "N" 'isearch-repeat-backward)
;; Move using visual lines
(define-key evil-motion-state-map "j" #'evil-next-visual-line)
(define-key evil-motion-state-map "k" #'evil-previous-visual-line)
(define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
(define-key evil-motion-state-map "^" #'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line)
;; Tab in normal mode works as tab in Emacs
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
;; Provide yank at insert time
(define-key evil-insert-state-map (kbd "C-y") 'yank)
;; Provide a visual-time shorcut to commenting
(define-key evil-visual-state-map "z" 'comment-region)
(define-key evil-visual-state-map "Z" 'uncomment-region)

(evil-mode 1)

;; make cursor look like Vim when in Vim normal moe
;;TODO: Move the following function to some utils
(defun def-assoc (key alist default)
  "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))  
(defun cofi/evil-cursor ()
  "Change cursor color according to evil-state."
  (let ((color-default "OliveDrab4")
        (colors '((insert . "dark orange")
		  (emacs . "sienna")
		  (visual . "white")))
	(cursor-default 'bar)
	(cursors '((visual . hollow)
		   (normal . box))))
    (setq cursor-type (def-assoc evil-state cursors cursor-default))
    (set-cursor-color (def-assoc evil-state cursors color-default))))
(setq evil-default-cursor #'cofi/evil-cursor)

;; windowing
;; (fill-keymap evil-window-map
;;     "C-g" nil
;;     ;; Splitting
;;     "\\" 'split-window-vertically
;;     "|" 'split-window-horizontally

;;     ;; Deleting
;;     "D" 'delete-window
;;     "C-d" 'delete-window
;;     "1" 'delete-other-windows

;;     ;; Sizing
;;     "RET" 'enlarge-window
;;     "-" 'shrink-window-horizontally
;;     "+" 'enlarge-window-horizontally

;;     ;; Moving
;;     ;"h" 'evil-window-left
;;     ;"j" 'evil-window-down
;;     ;"k" 'evil-window-up
;;     ;"l" 'evil-window-right

;;     ;; Swapping
;;     "M-h" 'swap-with-left
;;     "M-j" 'swap-with-down
;;     "M-k" 'swap-with-up
;;     "M-l" 'swap-with-right
;;     "S-<left>" 'swap-with-left
;;     "S-<down>" 'swap-with-down
;;     "S-<up>" 'swap-with-up
;;     "S-<right>" 'swap-with-right
;;     "SPC" 'swap-window

;;     ;; winner-mode
;;     "u" 'winner-undo
;;     "C-r" 'winner-redo
;;     ;; shadow rotating in evil-window-map
;;     "C-R" 'winner-redo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CEDET does a lot of stuff to the current settings, so we only wish to activate
; this when needed
(defun activate-cedet ()
  (interactive)
  (load "~/.emacs.d/cedet_setup.el")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-startup-indented t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       LATEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-parse-self nil)

; Other custom settings
(defun jsrn-latex-mode-hook ()
  (local-set-key (kbd "M-q") 'fill-sentence)  ; hard sentence wrap
  (setq fill-column 9999)            ; with hard senctence wrap, we don't want hard lines
  (visual-line-mode t)               ; but we do want visual word wrap
  (adaptive-wrap-prefix-mode t)      ; with adaptive indenting
  (setq LaTeX-item-indent 0)         ; indent \item as other stuff inside envs (works
			             ; better with adaptive-wrap-prefix-mode)
  (flyspell-mode t)
)
(add-hook 'LaTeX-mode-hook 'jsrn-latex-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dired displays less verbose information
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)

; Dired does not open a million buffers
(toggle-diredp-find-file-reuse-dir t)

(defun jsrn-dired-mode-hook ()
  ; Change dired-up-directory to find-alternate-file ..
  (lambda () (define-key dired-mode-map (kbd "^")
	       (lambda () (interactive) (find-alternate-file ".."))))
 )

; When Dired does something to a file, requiring a target, it suggests other open dired buffer
(setq dired-dwim-target 1)
; Load the advanced, not-touched-so-often stuff
(load "~/.emacs.d/dired_setup.el")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ELISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-emacs-lisp-mode-hook ()
  (auto-fill-mode t)
  (show-paren-mode t)
  (highlight-parentheses-mode t)
)
(add-hook 'emacs-lisp-mode-hook 'jsrn-emacs-lisp-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       FLYSPELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq flyspell-issue-message-flag nil)
(defun jsrn-spell-goto-next-and-suggest ()
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(setq flyspell-mode-map '(keymap
 (67108908 . jsrn-spell-goto-next-and-suggest)
 ))
(setq ispell-silently-savep t)

