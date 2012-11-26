;globally set user-interface
(set-face-attribute 'default nil :height 70)
(setq inhibit-splash-screen t)
(tool-bar-mode 0)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(fill-column 80)
 '(preview-gs-options (quote ("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(preview-image-type (quote dvipng))
  '(preview-scale-function 1))

; Other global nice options
(set-fringe-mode '(0 . 1)) ;activate only the right fringe area

; File type default modes
(add-to-list 'auto-mode-alist '("\\svg\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\env\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\scene\\'" . xml-mode))

; Load ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;match substr on what is written

; Load uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)


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
(require 'highlight-parentheses)
;; Other packages
(load "~/.emacs.d/fill-sentence.el")



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
  (setq fill-column 9999) ; with hard senctence wrap, we don't want hard lines
  (visual-line-mode t)        ; but we do want visual word wrap
  (adaptive-wrap-mode t)      ; with adaptive indenting
  (setq LaTeX-item-indent 0)  ; indent \item as other stuff inside envs (works
			      ; better with adaptive-wrap-mode)
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
