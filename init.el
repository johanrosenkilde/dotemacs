;; Keyboard layout to expect
(setq workman t)

;; About me
(setq user-full-name "Johan S. R. Nielsen"
      jsrn-user-mail-address "jsrn@jsrn.dk") ;; Std email; I will overwrite user-mail-address

;; Other global nice options
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode) ; set *scratch* buffer mode
(setq-default major-mode 'text-mode) ; set new buffers' major mode
(scroll-bar-mode -1) ;; Emacs gurus don't need no stinking scroll bars
(menu-bar-mode 0)    ;; or menu bars
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono-8"))
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(setq-default indent-tabs-mode nil) ; never insert tabs, do spaces
(setq compilation-scroll-output t
      grep-find-command "grep -r --exclude=.git "  ;; grep ignores Git
      visible-bell t
      split-height-threshold 9999  ;; never automatically split horisontally
      sentence-end-double-space nil  ;; sentences end with a dot, not with two spaces
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))  ;; autosaves put away
      backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
      
      )
(setq-default fill-column 80)
(setq tab-width 4)

;; Environment
(setenv "PATH" (concat (getenv "PATH") ":/home/jsrn/local/bin:/home/jsrn/code/scripts"))



;; Some font settings, extracted from Custom
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground
                         "black" :inverse-video nil :box nil :strike-through nil
                         :overline nil :underline nil :slant normal :weight
                         normal :height 90 :width normal :foundry "unknown"
                         :family "Monospace"))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t))))
 '(menu ((t (:height 1 :family "Monospace"))))
 '(table-cell ((t nil)))
)



(defadvice isearch-exit (after jsrn-goto-match-beginning activate)
  "After a search ends by RET, go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))
(defadvice isearch-repeat-forward (after jsrn-goto-match-beginning activate)
  "After a forwards search is repeated, go to beginning of match."
  (goto-char isearch-other-end))

;; File type default modes
(add-to-list 'auto-mode-alist '("\\.svg\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.scene\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sheet\\'" . sage-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Show startup-time after load
(add-hook 'after-init-hook (lambda ()
  (message (format "Emacs initialized: took %f seconds." (float-time (time-subtract after-init-time before-init-time))))))

;; Build and keep list of recent files
(recentf-mode t)
(setq recentf-save-file "~/.emacs.d/.recentf")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PACKAGE MANAGER / MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ELISP UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookup (key alist default)
  "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))  

(defun pour-mappings-to (map mappings)
  "Calls `define-key' with `map' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a define-key-understandable string and a interactive-fun."
  (dolist (mapping (-partition 2 mappings))
    (define-key map (car mapping) (cadr mapping)))
  map)

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (pour-mappings-to keymap mappings))

(defun fill-keymaps (keymaps &rest mappings)
  "Fill `KEYMAPS' with `MAPPINGS'.
See `pour-mappings-to'."
  (dolist (keymap keymaps keymaps)
    (let ((map (if (symbolp keymap)
                   (symbol-value keymap)
                 keymap)))
      (pour-mappings-to map mappings))))

(defun key-binding-other-window (keyb)
  "Run the given key binding in the other window"
  (interactive)
  (other-window 1)
  (condition-case err
      (funcall (key-binding keyb))
    (error (princ (format "Error: %s" err))))
  (other-window -1))

(defun next-in-list (ls obj)
  "Find the element in ls which is after obj. Returns the first
element of ls if obj is not in ls or is the last."
  (let* ((inlist (member obj ls)))
    (if (and inlist (cdr inlist))
         (car (cdr inlist))
       (car ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       GLOBALLY DEFINED CUSTOM FUNCTIONS AND KEYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make the useless I into the very useful X with Ctrl and Meta
(keyboard-translate ?\C-i ?\C-x)
(define-key key-translation-map (kbd "M-i") (kbd "M-x"))
;; Do the same such that emacsclient understands
(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (with-selected-frame frame (keyboard-translate ?\C-i ?\C-x))
             (define-key key-translation-map (kbd "M-i") (kbd "M-x"))
             ))

(global-set-key [(f1)] '(lambda ()
                          (interactive)
                          (woman (current-word))))
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
(global-set-key [(f2)] 'jsrn-recompile)
(global-set-key [(f3)] nil)
(global-set-key [(f4)] 'ffap) ;; look-up file at point
(global-set-key (kbd "M-?") 'hippie-expand)
(global-set-key (kbd "C-x m") 'ffap) ;; Find file at point (and override compose-mail)
(global-set-key (kbd "S-<backspace>") 'delete-horizontal-space)

;; Remove annoying shortcuts I sometimes press by mistake
(global-unset-key (kbd "C-x C-c")) ;; for killing emacs fast
(global-unset-key (kbd "C-h h")) ;; show the HELLO message
(global-unset-key (kbd "C-h C-c")) ;; show the COPYING message
(global-unset-key (kbd "C-h p")) ;; show the package Finder buffer
(global-unset-key (kbd "C-z")) ;; minimise Emacs
(global-unset-key (kbd "C-h C-f")) ;; show the FAQ
(global-unset-key (kbd "C-x n d")) ;; narrow to defun
(global-unset-key (kbd "C-x n p")) ;; narrow to page
(global-unset-key (kbd "C-x n n")) ;; narrow to region


(defun kill-line-backwards ()
  "Kill the current line backwards from the current column.

Kill the current line backwards from the current column. If at col 0, kill
only the newline character"
  (interactive)
  (if (= (current-column) 0) ; If we are at beginning, kill newline char
      (backward-delete-char 1)
    (kill-line 0)))
(global-set-key (kbd "M-<backspace>") 'kill-line-backwards)

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
(global-set-key (kbd "C-a") 'beginning-of-visual-line-smart) ;Override default C-a

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
(global-set-key [f11] 'toggle-fullscreen)

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
(global-set-key [f1] 'show-clock)

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
        (princ (format "Local key map:\t\t`%s'\t\t*DEFINED*\n\n" local-key))
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

(global-set-key (kbd "C-x C-=") '(lambda () (interactive) (modify-font-height 10)))
(global-set-key (kbd "C-x C--") '(lambda () (interactive) (modify-font-height -10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       GLOBAL ACTIVATION OF UBIQUITOUS PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern list functionality + more
(eval-after-load "dash" '(dash-enable-font-lock))
(require 'dash)

;; minor mode Highlight parentheses which are around cursor
(require 'highlight-parentheses)

;; Uniquify gives better names to buffers containing files with same base name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

(require 'help+)
(require 'help-fns+)
(require 'help-mode+)

;; sudo support and others
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(require 'tramp)
;; From djcb
(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with root-privileges
using tramp/sudo, if the file is not writable by user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
(global-set-key (kbd "C-x C-S-f") 'find-file-as-root)

;; Jump as an ace
(require 'ace-jump-mode)
(setq ace-jump-mode-scope 'window)

;; Load diminish (though diminish of usual suspects is done at the end)
(require 'diminish)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       AUTO-COMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'pos-tip)
(defun jsj-ac-show-help () ; stolen on the net
  "show docs for symbol at point or at beginning of list if not on a symbol"
  (interactive)
  (let ((s (save-excursion
             (or (symbol-at-point)
                 (progn (backward-up-list)
                        (forward-char)
                        (symbol-at-point))))))
    (pos-tip-show (if (equal major-mode 'emacs-lisp-mode)
                      (ac-symbol-documentation s)
                    (ac-slime-documentation (symbol-name s)))
                  'popup-tip-face
                  ;; 'alt-tooltip
                  (point)
                  nil
                  -1)))
(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key lisp-mode-shared-map (kbd "C-c C-h") 'jsj-ac-show-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       IDO MORE STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(require 'smex)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t) ;match substr on what is written
(setq ido-use-filename-at-point nil)
(setq ido-file-extensions-order '(".tex" ".sage" ".py" ".bib" ".txt"))
(setq ido-auto-merge-work-directories-length -1) ; don't suggest stuff in other dirs
(global-set-key "\M-x" 'smex) ;; awesome function chooser
(add-to-list 'ido-ignore-buffers "*terminal")
(ido-mode t)

;; Use smex for C-h f
(defun  smex-describe-function (fun &optional commandp)
  "As `describe-function' but use smex completion."
  (interactive
   (list (let* ((fn (or (and (fboundp 'symbol-nearest-point)
                             (symbol-nearest-point))
                        (function-called-at-point)))
                (smex-prompt-string "Describe function: "))
           (smex-completing-read (if fn (cons (symbol-name fn) list-of-all-functions) list-of-all-functions) nil))))
  (describe-function (intern fun))
  )
(global-set-key (kbd "C-h f") 'smex-describe-function)

(load "ido_goto_symbol")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ADMINISTRATIVE MODE
;; My own created meta mode for loading various stuff for the emacs
;; window which will do email and agenda.
;; Non-reversibly changes the current window's behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq administrative-mode-hook ())
(defun administrative-mode ()
  (interactive)
  (require 'administrative_setup "administrative_setup.el")
  (run-hooks 'administrative-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SECRETS: Password management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-secret-activate ()
  (interactive)
  (require 'simple-secrets)
  (secret-load-keys)
  (evil-global-set-key 'normal (kbd "C-1") 'secret-lookup-clipboard)
  (evil-global-set-key 'emacs (kbd "C-1") 'secret-lookup-clipboard)
  (evil-global-set-key 'normal (kbd "C-2") 'secret-new)
  (evil-global-set-key 'emacs (kbd "C-2") 'secret-new)
  )
(add-hook 'administrative-mode-hook 'jsrn-secret-activate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       WORKMAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (eq workman t)
    ; Workman homerow movement
    (progn
      (setq evil-left-key "y"
            evil-right-key "o"
            evil-up-key "e"
            evil-down-key "n"
            )
      )
  ; Qwerty homerow movement
  (setq evil-left-key "h"
          evil-right-key "l"
          evil-up-key "k"
          evil-down-key "j"
          ))
(setq evil-left-key-uc  (upcase evil-left-key)
      evil-right-key-uc (upcase evil-right-key)
      evil-up-key-uc    (upcase evil-up-key)
      evil-down-key-uc  (upcase evil-down-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       EVIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(setq-default evil-symbol-word-search t)
(setq evil-find-skip-newlines t
      evil-move-cursor-back nil
      evil-ex-search-highlight-all nil
      evil-want-fine-undo t)
(setq evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))
;; Escape quits anything
(fill-keymaps (list evil-normal-state-map
                    evil-visual-state-map)
              [escape] 'keyboard-quit)
;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(fill-keymaps (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map)
              [escape] 'minibuffer-keyboard-quit)
(fill-keymap isearch-mode-map [escape] 'isearch-cancel)
(global-set-key [escape] 'evil-exit-emacs-state)
(fill-keymap evil-emacs-state-map
             [escape] 'keyboard-quit)


;; Key-bindings in all modes
(fill-keymaps (list evil-normal-state-map
                    evil-visual-state-map
                    evil-insert-state-map)
              ;; TAB is overtaken by Emacs (which is ok), so map jump-forward to C-Esc
              (kbd "C-<escape>") 'evil-jump-forward
              (kbd "C-e")        'end-of-visual-line
              (kbd "C-a")        'beginning-of-visual-line-smart)
;; Key-bindings in normal mode
(defun jsrn-goto-first-symbol-use ()
  (interactive)
  (let ((sym (evil-find-symbol nil)))
    (evil-goto-first-line)
    (search-forward-regexp (format "\\_<%s\\_>" (regexp-quote sym)))
    (evil-backward-word-begin)
    ))
(fill-keymap evil-normal-state-map
             (kbd "A") (lambda () (interactive) (end-of-visual-line) (evil-insert-state))
             ;; Search using Emacs' isearch but using Vim keybindings
             "/" 'isearch-forward
             "?" 'isearch-backward
             "N" 'isearch-repeat-backward
             (kbd "C-#") 'jsrn-goto-first-symbol-use
             (kbd "M-p") (lambda () (interactive) (evil-paste-pop -1))
             ;; Tab in normal mode works as tab in Emacs
             (kbd "TAB") 'indent-for-tab-command
             (kbd "C-y") 'yank
             (kbd "M-n") 'ido-goto-symbol
             (kbd "M-,") 'imenu-anywhere
             (kbd "z d") (lambda () (interactive) (kill-buffer (current-buffer))) ;; kill current buffer
             )
;; Key-bindings in insert mode
(fill-keymap evil-insert-state-map
             (kbd "<return>") 'newline-and-indent
             (kbd "C-y") 'yank
             (kbd "C-p") 'evil-paste-pop)
;; Key-bindings in visual mode
(fill-keymap evil-visual-state-map
             "v" 'mark-current-line-smart
             ;; Provide a visual-time shorcut to commenting
             "z" 'comment-region
             "Z" 'uncomment-region
             "#" 'search-region-backward
             "*" 'search-region-forward
             )

; Remenber positions when searching so they can be found in jump-point-ring
(defadvice isearch-forward (before marker activate)
  "Store current position in jump list"
  (evil-set-jump))
(defadvice isearch-repeat-forward (before marker activate)
  "Store current position in jump list"
  (evil-set-jump))
(defadvice isearch-backward (before marker activate)
  "Store current position in jump list"
  (evil-set-jump))
(defadvice isearch-repeat-backward (before marker activate)
  "Store current position in jump list"
  (evil-set-jump))

;; Some motions
(evil-declare-motion 'backward-block)
(evil-declare-motion 'forward-block)
(evil-declare-motion 'ace-jump-mode)
; Enable//Disable Evil in certain modes
(cl-loop for (mode . state) in '(
                              (eassist-mode . emacs)
                              (xgtags-select-mode . emacs)
                              (magit-branch-manager-mode . emacs)
                              (reftex-select-label-mode . emacs)
                              (inferior-sage-mode . emacs)
                              (inferior-python-mode . emacs)
                              (debugger-mode . emacs)
                              (shell-mode . emacs)
                              (diff-mode . emacs)
                              (multi-term-mode . emacs)
                              (undo-tree-visualizer-mode . emacs)
                              ;; Disable
                              (completion-list-mode . normal)
                              ;; Disable strange motion state
                              (help-mode . normal)
                              (apropos-mode . normal)
                              (Info-mode . normal)
                              (woman-mode . normal)
                              (compilation-mode . normal)
                              (git-commit-mode . normal)
                              )
      do (evil-set-initial-state mode state))

(evil-mode 1)
;; Remove certain keybindings
(fill-keymaps (list evil-normal-state-map evil-visual-state-map evil-insert-state-map)
              (kbd "C-.") nil
              )

;; In some modes, the special Enter is most I need, so map this to <enter>.
(evil-declare-key 'motion woman-mode-map (kbd "<return>") 'woman-follow)
(evil-declare-key 'motion reftex-toc-mode-map (kbd "<return>") 'reftex-toc-goto-line-and-hide)
(evil-declare-key 'motion finder-mode-map (kbd "<return>") 'finder-select)
(evil-declare-key 'motion completion-list-mode-map (kbd "<return>") 'choose-completion)
(evil-declare-key 'insert shell-mode-map (kbd "<return>") 'comint-send-input)
;; Same for quit q
(evil-declare-key 'normal woman-mode-map "q" 'Man-quit)
(evil-declare-key 'normal reftex-toc-mode-map "q" 'reftex-toc-quit)
(evil-declare-key 'normal completion-list-mode-map "q" 'quit-window)
(evil-declare-key 'normal help-mode-map "q" 'quit-window)
(evil-declare-key 'normal Info-mode-map "q" 'quit-window)

;; make cursor look like Vim when in Vim normal mode
(defun cofi/evil-cursor ()
  "Change cursor color according to evil-state."
  (let ((color-default "OliveDrab4")
        (colors '((insert . "dark orange")
                  (emacs . "sienna")
                  (visual . "white")))
        (cursor-default 'bar)
        (cursors '((visual . hollow)
                   (normal . box))))
    (setq cursor-type (lookup evil-state cursors cursor-default))
    (set-cursor-color (lookup evil-state cursors color-default))))
(setq evil-default-cursor #'cofi/evil-cursor)
 

;; windowing
(defun jsrn-delete-window-below ()
  (interactive)
  )

(defun expand-window-vertically ()
  "Expand current window vertically by deleting the window just below, or the
one above if there are no windows below"
  (interactive)
  (setq is-lowest-window nil)
  (condition-case err
      (progn
        (evil-window-down 1)
        (when (string-match "Minibuf" (buffer-name))
          (progn
            (setq is-lowest-window t)
            (evil-window-up 1))))
    (error (setq is-lowest-window t)))
  (when is-lowest-window
      (evil-window-up 1))
  (delete-window)
  )

(fill-keymap evil-window-map
             ;; Moving (these exist for Qwerty)
             evil-left-key  'evil-window-left
             evil-down-key  'evil-window-down
             evil-up-key    'evil-window-up
             evil-right-key 'evil-window-right
             (kbd "C-g") nil
             ;; Splitting
             "\\" 'split-window-horizontally
             "/" 'split-window-vertically
             ;; Deleting
             (kbd "C-d") 'delete-window
             "1" 'delete-other-windows
             ;; Sizing
             (kbd "RET") 'enlarge-window
             ;; Buffer switching
             "p"         'switch-to-prev-buffer
             "P"         'switch-to-next-buffer
             (kbd "C-p") (lambda () (interactive)
                            (switch-to-buffer-other-window nil))
             ;; Moving
             evil-left-key  'evil-window-left
             evil-down-key  'evil-window-down
             evil-up-key    'evil-window-up
             evil-right-key 'evil-window-right
             (kbd "C-w")    'evil-window-mru  ;; go to last accessed (swap back/forth)
             (kbd "v")      'expand-window-vertically
             ;; override C-w C-o/n since it is easy to type when wanting C-w o/n
             (kbd "C-o")    'evil-window-right 
             (kbd "C-n")    'evil-window-down
             )

;; Put all window bindings in emacs and insert state also
(define-key evil-emacs-state-map (kbd "C-w") evil-window-map) 
(define-key evil-insert-state-map (kbd "C-w") evil-window-map)

;;??? This is strangely needed
(fill-keymap evil-insert-state-map (kbd "C") 'self-insert-command) 

(evil-define-motion jsrn-scroll-down ()
  "Scroll down half a page and recenter"
  :type inclusive
  :jump t
  (move-to-window-line (- (evil-num-visible-lines) 2))
  (recenter)
  )
(evil-define-motion jsrn-scroll-up ()
  "Scroll up half a page and recenter"
  :type inclusive
  :jump t
  (move-to-window-line 2)
  (recenter)
  )
(fill-keymaps (list evil-motion-state-map evil-normal-state-map)
	      evil-left-key  'evil-backward-char
	      evil-right-key 'evil-forward-char
	      evil-up-key    'evil-previous-visual-line
	      evil-down-key  'evil-next-visual-line
              "B"            'evil-backward-WORD-end
              "l"            'evil-forward-word-end
              "L"            'evil-forward-WORD-end
              "$"            'evil-end-of-visual-line
              "^"            'evil-first-non-blank-of-visual-line
              (kbd "C-b")    'jsrn-scroll-up
              (kbd "S-SPC")  'jsrn-scroll-up
              (kbd "C-d")    'jsrn-scroll-down
              (kbd "SPC")    'jsrn-scroll-down
              (kbd "C-w SPC")   '(lambda () (interactive) (key-binding-other-window (kbd "SPC")))
              (kbd "C-w S-SPC") '(lambda () (interactive) (key-binding-other-window (kbd "S-SPC")))
              (kbd "C-w TAB")   '(lambda () (interactive) (next-error))
              (kbd "C-w G")     '(lambda () (interactive) (key-binding-other-window (kbd "G")))
              (kbd "C-w g")     '(lambda () (interactive) (key-binding-other-window (kbd "gg")))
              (kbd "C-f")    'ace-jump-mode
	      )

;; Workman fixes 
(defun fix-evil-workman ()
  "Fix Evil layout problems due to Workman"
  (interactive)
  (fill-keymap evil-normal-state-map
               "h"   'evil-open-below
               "H"   'evil-open-above
               "Y"   'evil-window-top
               "U"   'evil-yank-line
               )
  (fill-keymaps (list evil-normal-state-map evil-visual-state-map evil-normal-state-local-map)
                "k"   'isearch-repeat-forward
                "K"   'isearch-repeat-backward
                "j"   'evil-yank)
                                    ;TODO: the above seems to be reverted by Evil once in a while
  (fill-keymap evil-visual-state-map
               "o"   'evil-forward-char
               "l"   'exchange-point-and-mark)
)

(if workman (fix-evil-workman))

;; Evil-numbers (Vim-like increment and decrement)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SURROUND-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setup-org ()
  (require 'org_setup "org_setup.el"))
(add-hook 'org-mode-hook 'setup-org)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       LATEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setup-latex ()
  (require 'latex_setup "latex_setup.el"))
(add-hook 'LaTeX-mode-hook 'setup-latex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired_setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       COMPILATION-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-compilation-mode-hook ()
  (local-unset-key "g") ;; disable "recompile" command to reinstate Evil's g
  )
(add-hook 'compilation-mode-hook 'jsrn-compilation-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-emacs-lisp-mode-hook ()
  (setq evil-shift-width 2)
  (auto-fill-mode t)
  (show-paren-mode t)
  (highlight-parentheses-mode t)
  (require 'paredit)
  (paredit-mode)
  (fill-keymap evil-normal-state-local-map
    "D" 'paredit-kill
    "s" 'forward-sexp
    "S" 'backward-sexp
    "Q" (lambda () (interactive) (up-list -1))
    )
  )
(add-hook 'emacs-lisp-mode-hook 'jsrn-emacs-lisp-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       FLYSPELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'flyspell-mode "flyspell_setup.el" "Flyspell" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DESKTOP (session management)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'desktop)
(require 'mdesktop)
(setq history-length 250)
(setq desktop-save t) ; don't ask, just act
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-base-file-name "desktop")
(global-set-key [(f8)] 'mdesktop-switch)
(add-hook 'auto-save-hook '(lambda ()
                             (if mdesktop-current
                                 (mdesktop-save-current) ; save desktop ever so often
                                 )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq python-is-set-up nil)
(defun setup-python ()
  (require 'python_setup "python_setup.el"))
;; (add-hook 'python-mode-hook 'setup-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'sage-mode "sage_setup.el" "Major mode for Sage" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       IPYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ipython-notebook "ipython_setup.el" "Browse and open an IPython notebook" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       OCAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ocaml-mode "ocaml_setup.el" "Major mode for Ocaml" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;       HASKELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-haskell-mode-hook ()
  ;; (turn-on-haskell-doc-mode "haskell-doc" nil t)
  (turn-on-haskell-indentation)
  (setq haskell-process-type 'cabal-repl)
  (interactive-haskell-mode)
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  )
(add-hook 'haskell-mode-hook 'jsrn-haskell-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       FSHARP F#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'fsharp-mode "fsharp_setup.el" "Major mode for F-sharp" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       C/C++ AND GDB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(defun cpp-setup-hook ()
  ; Cannot use autoload since c++-mode is born in Emacs
  (require 'cpp_setup "cpp_setup.el")
  )
(add-hook 'c++-mode-hook 'cpp-setup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ANKI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'anki-mode "anki_setup.el" "Major mode for writing Anki word lists" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SMTPMAIL AND MU4E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setup-mail ()
  (require 'mail_setup "mail_setup.el"))
(add-hook 'administrative-mode-hook 'setup-mail)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       OTHER MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text-mode
(add-hook 'text-mode-hook (lambda () (visual-line-mode)))

;; Undo-tree mode
(define-key undo-tree-visualizer-mode-map (kbd "n") 'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-mode-map (kbd "e") 'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map (kbd "y") 'undo-tree-visualize-switch-branch-left)
(define-key undo-tree-visualizer-mode-map (kbd "o") 'undo-tree-visualize-switch-branch-right)

;; Web mode (HTML, PHP)
(defun jsrn-web-mode-hook ()
  (define-key web-mode-map (kbd "C-c .") 'web-mode-mark-and-expand)
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook 'jsrn-web-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       MTG (MAGIC) LIST MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'mtg-list-mode "mtg_setup.el" "Major mode for writing MTG lists" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       TERMINAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'get-term "terminal_setup.el" "Fire up a terminal" t)
(global-set-key [(f9)] 'get-term)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-diff-mode-hook ()
  (interactive)
  (fill-keymap evil-motion-state-local-map
               evil-down-key 'diff-hunk-next
               evil-up-key   'diff-hunk-prev
               "q"           'kill-buffer)
  )
(add-hook 'diff-mode-hook 'jsrn-diff-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       MAGIT & MONKY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'magit-status "magit_setup.el" "Git repository status using Magit" t )
(global-set-key [(f12)] 'magit-status)
(autoload 'monky-status "monky_setup.el" "Mercurial repository status using Monky" t )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ASYMPTOTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/share/texmf-dist/asymptote/")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
(defun jsrn-asy-mode-hook ()
  (interactive)
  (setq ps-view-command "okular"
        asy-command "asy -V -psviewer=okular" )
  (fill-keymap asy-mode-map
               (kbd "C-c C-h") 'asy-show-function-at-point)
  )
(add-hook 'asy-mode-hook 'jsrn-asy-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       LEDGER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun setup-ledger ()
;;   (require 'ledger_setup "ledger_setup.el"))
;; (add-hook 'ledger-mode-hook 'setup-ledger)
(autoload 'ledger-mode "ledger_setup.el" "Load ledger setup")
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PRINTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'printing)		; load printing package
(setq jsrn-pr-printer-alist
      '((et2-006 "lpr"     nil "et2-color-konica-006")   ;; Ulm University
        ))
(setq jsrn-current-printer 'et2-006)
(defun jsrn-pr-set-printers ()
  (interactive)
  (setq pr-path-alist
        '((unix      "." ghostview mpage PATH)
          (ghostview "/usr/bin/gv")
          (mpage     "/usr/bin/mpage")
          ))
  (setq pr-txt-printer-alist jsrn-pr-printer-alist)
  (setq pr-ps-printer-alist jsrn-pr-printer-alist)
  (setq pr-txt-name  jsrn-current-printer)
  (setq pr-ps-name  jsrn-current-printer)
  (pr-update-menus t)		; update now printer and utility menus
)
(jsrn-pr-set-printers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIMINISH USUAL SUSPECTS (Cleaning up mode line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-loop for minor-mode in '(undo-tree-mode
                          auto-fill-function
                          visual-line-mode
                          highlight-parentheses-mode)
      do (diminish minor-mode))
