;; Keyboard layout to expect
(setq workman t)

;; About me
(setq user-full-name "Johan S. R. Nielsen"
      jsrn-user-mail-address "jsrn@jsrn.dk") ;; Std email; I will overwrite user-mail-address

;; Other global nice options
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq-default major-mode 'text-mode)
(scroll-bar-mode -1) ;; Emacs gurus don't need no stinking scroll bars
(menu-bar-mode 0)    ;; or menu bars
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-8"))
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(setq compilation-scroll-output t)
(setq-default indent-tabs-mode nil) ; never insert tabs, do spaces
(setq grep-find-command "grep -r --exclude=.git ") ;; grep ignores Git
(setq visible-bell t)
(setq split-height-threshold 9999) ;; never automatically split horisontally
(setq sentence-end-double-space nil) ;; sentences end with a dot, not with two spaces

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ELISP UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-assoc (key alist default)
  "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))  

(defun take (n lst)
  "Return atmost the first `N' items of `LST'."
  (let (acc '())
    (while (and lst (> n 0))
      (setq n (1- n))
      (push (car lst) acc)
      (setq lst (cdr lst)))
    (nreverse acc)))

(defun find-first (lst pred)
  "Return first element of list matching predicate or nil.
Note: there is no way of discerning between a success of a 'nil' element and a failure."
  (setq running t)
  (setq res nil)
  (while (and running lst)
    (if (funcall pred (car lst))
      (progn
       (setq running nil)
       (setq res (car lst)))
      (setq lst (cdr lst))
    ))
  res)

(defun group (lst n)
  "Group `LST' into portions of `N'."
  (let (groups)
    (while lst
      (push (take n lst) groups)
      (setq lst (nthcdr n lst)))
    (nreverse groups)))

(defun pour-mappings-to (map mappings)
  "Calls `define-key' with `map' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a define-key-understandable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
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
(global-set-key [(f4)] 'ffap) ;; look-up file at point
(global-set-key "\M-?" 'hippie-expand)
(global-set-key (kbd "C-x m") 'ffap) ;; Find file at point (and override compose-mail)
(global-set-key (kbd "S-<backspace>") 'delete-horizontal-space)

;; Remove annoying shortcuts I sometimes press by mistake
(global-unset-key (kbd "C-x C-c")) ;; for killing emacs fast
(global-unset-key (kbd "C-h h")) ;; show the HELLO message
(global-unset-key (kbd "C-h C-c")) ;; show the COPYING message
(global-unset-key (kbd "C-h p")) ;; show the package Finder buffer
(global-unset-key (kbd "C-z")) ;; minimise Emacs


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

;;TODO: delete-visual-line to replace S-d

;;Function for reloading the .emacs file
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))

(defun delete-process-interactive ()
  (interactive)
  (let ((pname (ido-completing-read "Process Name: " 
                    (mapcar 'process-name (process-list)))))

    (delete-process (get-process pname))))

(defun mark-current-block (&optional delim)
    "Find last delimiter line, set mark and then go to next delimiter
line. Return the set mark.
If no argument is given, a delimiter line is a blank line. Otherwise, it is a
line starting with the string given as the argument."
    (let ((ldelim (if delim (concat "^" delim) "^$")))
      (search-backward-regexp ldelim nil 0)
      (let ((beg (point)))
        (push-mark)
        (next-line)
        (search-forward-regexp ldelim nil 0)
        (goto-char (match-beginning 0))
        (backward-char)
        beg
        )))
(defun mark-current-block-i (delim)
  (interactive "sDelimiting lines match from start (default is empty line): ")
  (let ((delim (if (string-equal delim "") "$" delim)))
    (mark-current-block delim)
  ))

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
(global-set-key [f6] 'show-clock)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PACKAGE-INSTALL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Other packages
;; Fill sentence: reflows paragraph to have only linebreaks at sentence boundaries
(load "fill-sentence.el")

;; minor mode Highlight parentheses which are around cursor
(require 'highlight-parentheses)

;; Uniquify gives better names to buffers containing files with same base name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Winner gives undo and redo of windows arrangements
(require 'winner)
(winner-mode 1)
(global-set-key (kbd "M-S-<left>") 'winner-undo)
(global-set-key (kbd "M-S-<right>") 'winner-redo)

;; Build and keep list of recent files
(recentf-mode 1)

;; sudo support and others
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
(ido-mode t)
(setq ido-enable-flex-matching t) ;match substr on what is written
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".tex" ".sage" ".py" ".bib" ".txt"))
(setq ido-auto-merge-work-directories-length -1) ; don't suggest stuff in other dirs
(global-set-key "\M-x" 'smex) ;; awesome function chooser
(add-to-list 'ido-ignore-buffers "*terminal")
(load "ido_goto_symbol")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ADMINISTRATIVE MODE
;; My own created meta mode for loading various stuff for the emacs
;; window which will do email and agenda.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq administrative-mode-hook ())
(defun administrative-mode ()
  (interactive)
  (global-set-key [(f10)] 'org-agenda-list)
  (run-hooks 'administrative-mode-hook))


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
             "Z" 'uncomment-region)

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
    (setq cursor-type (def-assoc evil-state cursors cursor-default))
    (set-cursor-color (def-assoc evil-state cursors color-default))))
(setq evil-default-cursor #'cofi/evil-cursor)
 

;; windowing
(defun jsrn-delete-window-below ()
  (interactive)
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
             (kbd "C-w")    'evil-window-prev
             (kbd "v")    (lambda ()  (interactive) (evil-window-down 1) (delete-window))
             ;; override C-w C-o/n since it is easy to type when wanting C-w o/n
             (kbd "C-o")    'evil-window-right 
             (kbd "C-n")    'evil-window-down
             )

;; Put all window bindings in emacs state also
(define-key evil-emacs-state-map (kbd "C-w") evil-window-map) 

;;??? This is strangely needed
(fill-keymap evil-insert-state-map (kbd "C") 'self-insert-command) 

(defun jsrn-scroll-down ()
  (interactive)
  (when (eq 1 (point))
    (push-mark)
    (evil-scroll-down nil))
  (push-mark)
  (evil-scroll-down nil)
  (recenter)
  )
(defun jsrn-scroll-up ()
  (interactive)
  (push-mark)
  (evil-scroll-up nil)
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
              (kbd "C-w SPC")  'scroll-other-window
              (kbd "C-w S-SPC")'scroll-other-window-down
              (kbd "C-f")    'ace-jump-mode
	      )

;; Workman fixes 
(if workman
    (progn
      (fill-keymap evil-normal-state-map
                   "h"   'evil-open-below
                   "H"   'evil-open-above
                   "Y"   'evil-window-top
                   "U"   'evil-yank-line
                   "k"   'isearch-repeat-forward
                   "K"   'isearch-repeat-backward
                   )
      (fill-keymaps (list evil-normal-state-map evil-visual-state-map)
                    "j"   'evil-yank)
                                        ;TODO: the above seems to be reverted by Evil once in a while
      (fill-keymap evil-visual-state-map
                   "o"   'evil-forward-char
                   "l"   'exchange-point-and-mark)
    )
)


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
(setq org-startup-indented t)
(defun jsrn-agenda-activate ()
  "Activate the current Emacs as an agenda Emacs. Weird stuff seem to happen
sometimes if more than one Emacs has this set"
  (interactive)
  (find-file "~/orgs/home.org")
  ;; Set files which contains agenda files to all .org files in specified dir
  (setq org-agenda-files (directory-files "~/orgs" t ".org$" t))
  ;; Various agenda setup
  (setq org-agenda-repeating-timestamp-show-all nil) ; don't show repititions in agenda
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
  (defun appt-disp-window (mins curtime text)
    "Redefine Appointment reminder function to show a Memo using system call"
    (call-process "/usr/bin/notify-send" nil nil nil (format "Appointment:\n%s \n in  %s min" text mins)))
  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" nil 'jsrn-org-agenda-to-appt)
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
  (defun jsrn-org-up-element ()
    (interactive)
    (push-mark)
    (org-up-element))
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
               (kbd "C-c a") 'org-agenda)
  (fill-keymaps (list evil-motion-state-local-map
                      evil-normal-state-local-map)
                (kbd "~")  'jsrn-org-up-element)
  ;; Let winner keys overwrite org-mode
  (define-key evil-normal-state-local-map (kbd "M-S-<left>") 'winner-undo) 
  (define-key evil-normal-state-local-map (kbd "M-S-<right>") 'winner-redo)
  )
(add-hook 'org-mode-hook 'jsrn-org-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       LATEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "latex_setup")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired displays less verbose information
(require 'ls-lisp)
(require 'dired)
(setq ls-lisp-use-insert-directory-program nil)
;; Dired does not open a million buffers
(toggle-diredp-find-file-reuse-dir 1)
;; When Dired does something to a file, requiring a target, it suggests other open dired buffer
(setq dired-dwim-target 1)
;; Dired doesn't show dot-files per default. Use C-u s <Ret> to change
(setq dired-listing-switches "-l")

(defun jsrn-dired-mode-hook ()
  ;; Highlight current line
  (hl-line-mode)
  (defun jsrn-dired-up-directory ()
    "Go up dir without opening new buffer"
    (interactive)
    (find-alternate-file ".."))
  (evil-define-key 'normal dired-mode-map "^" 'jsrn-dired-up-directory)
  (evil-define-key 'normal dired-mode-map "J" 'dired-goto-file)
  (evil-define-key 'normal dired-mode-map "K" 'dired-do-kill-lines)
  (evil-define-key 'normal dired-mode-map "e" 'diredp-previous-line)
  (evil-define-key 'normal dired-mode-map "k" 'isearch-repeat-forward)
  (evil-define-key 'normal dired-mode-map "r" 'dired-do-redisplay)
  (fill-keymap evil-normal-state-local-map
               (kbd "SPC") 'jsrn-scroll-down
               (kbd "S-SPC") 'jsrn-scroll-up)
  )
(add-hook 'dired-mode-hook 'jsrn-dired-mode-hook)
;; Load the advanced, not-touched-so-often stuff
(load "dired_setup")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ELISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-emacs-lisp-mode-hook ()
  (setq evil-shift-width 2)
  (auto-fill-mode t)
  (show-paren-mode t)
  (highlight-parentheses-mode t)
  )
(add-hook 'emacs-lisp-mode-hook 'jsrn-emacs-lisp-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       FLYSPELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(setq ispell-dictionary "british")
;; Cycle through dictionaries. First make the language ring
(let ((langs '("british" "dansk" )))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun jsrn-cycle-dictionary ()
  (interactive)
  (let* ((cur (if (or (not (boundp 'ispell-local-dictionary)) (eq nil ispell-local-dictionary))
                 (ring-ref lang-ring -1)
               ispell-local-dictionary))
         (new (ring-next lang-ring cur)))
    (progn
      (ispell-change-dictionary new)
      (message "Changed dictionary to %s" new)
      )))
(defun jsrn-spell-goto-next-and-suggest ()
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(define-key flyspell-mode-map [(control ?\.)] 'jsrn-spell-goto-next-and-suggest)
(define-key flyspell-mode-map [(control ?\,)] nil)
(define-key flyspell-mode-map [(f6)] 'jsrn-cycle-dictionary)

(setq ispell-silently-savep t)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pretty-lambdada) ;typeset word "lambda" as the symbol
(defun jsrn-python-mode-hook ()
  (interactive)
  (pretty-lambda-mode 1)
  )

(add-hook 'python-mode-hook 'jsrn-python-mode-hook)
(add-hook 'sage-mode-hook #'pretty-lambda-mode 1)

;; Rope and ropemode
(setq python-custom-libs (list "rope" "ropemode" "Pymacs" "ropemacs"))
(setq pythonpath "")
(cl-loop for lib in python-custom-libs
         do (setq pythonpath (concat pythonpath (if (string-equal "" pythonpath)
                                                    "" ":") "/home/jsrn/local/"
                                                    lib "/build/lib/")))
(setenv "PYTHONPATH" pythonpath)
         
;; Pymacs
(add-to-list 'load-path "/home/jsrn/local/Pymacs/")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(setenv "PYMACS_PYTHON" "python2")
(setq pymacs-load-path '("/home/jsrn/local/rope/build/lib/rope"
                         "/home/jsrn/local/ropemacs/build/lib/ropemacs"
                         "/home/jsrn/local/ropemode/build/lib/ropemode"
                         ))
(require 'pymacs)

;; Load ropemacs
(pymacs-load "ropemacs" "rope-")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))
(setq sage-path "/home/jsrn/local/sage/sage-5.13")
;;(setq sage-mode-path (cl-concatenate 'string sage-path "/local/share/emacs"))
(setq sage-mode-path "/home/jsrn/local/sage-mode/sage-mode-jsrn/emacs")
(add-to-list 'load-path sage-mode-path)
(require 'sage "sage")
(require 'sage-view "sage-view")
(require 'sage-blocks "sage-blocks")
(add-hook 'sage-startup-after-prompt-hook 'sage-view)
(setq sage-command (cl-concatenate 'string sage-path "/sage"))
;; (defun jsrn-sage-mode-hook ()
;;   (interactive)
;;   )
;; (add-hook 'sage-mode-hook 'jsrn-sage-mode-hook)

(defun jsrn-inferior-sage-mode-hook ()
  (interactive)
  (fill-keymap evil-insert-state-local-map
               (kbd "<return>") 'comint-send-input)
  )
(define-key inferior-sage-mode-map (kbd "C-SPC") 'jsrn-scroll-up)
(define-key inferior-sage-mode-map (kbd "M-C-SPC") 'jsrn-scroll-down)
(define-key sage-mode-map (kbd "C-c C-h") 'sage-pcomplete-or-help)
(defun sage-refind-sage ()
  "Ensure that the local buffer's sage points to a running process. Otherwise,
  find a running sage process for it, or return nil"
  (interactive)
  (if (and sage-buffer (buffer-name sage-buffer))
      sage-buffer
    (progn
      (let ((buf (find-first (buffer-list) (lambda (buf)
                                             (string-match "Sage-main"
                                                           (buffer-name buf))))
                 ))
        (when buf
          (setq sage-buffer buf))
        buf))
  ))
(defadvice sage-send-buffer (before sage-send-region-refind-sage activate)
  (sage-refind-sage))
(defadvice sage-send-region (before sage-send-region-refind-sage activate)
  (sage-refind-sage))
(defun sage-send-class ()
  (interactive)
  (save-excursion
    (search-backward-regexp "^class ") ; find a class line or error
    (let ((begin (point)))
      (push-mark) ; for history jumping
      (next-line)
      (when (search-forward-regexp "^[^ \\t\n]" nil 1) ; find first non-indented line
        (backward-char)) ; go to right before if we are not at file end
      (sage-send-region begin (point))
      )))
(defun sage-restart ()
  (interactive)
  (when (buffer-name (sage-refind-sage)) ; test if sage-buffer is defined and not killed
    ;; get the sage process and unset its query flag
      (set-process-query-on-exit-flag (get-buffer-process sage-buffer) nil)
      (kill-buffer sage-buffer)
      (setq sage-buffer nil))
  (sage))
(fill-keymap sage-mode-map
             (kbd "C-c C") 'sage-send-class
             (kbd "C-c C-z") '(lambda () (interactive) 
                                (switch-to-buffer-other-window (sage-refind-sage)))
             )
(add-hook 'inferior-sage-mode-hook 'jsrn-inferior-sage-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       FSHARP F#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsrn-fsharp-mode-hook ()
  (setq evil-shift-width 2)
  (electric-pair-mode)
  (column-number-mode)
  (defun fsharp-send-current-block ()
    "Find last blank line and next blank line, and send all in between to Sage buffer"
    (interactive)
    (save-excursion
      (evil-backward-paragraph)
      (let ((beg (point)))
        (evil-forward-paragraph)
        (fsharp-eval-region beg (point))
      ))
    )
  (defun jsrn-fsharp-load-files (files)
    "Reload each file of the list of files into the inferior buffer"
    (interactive)
    (save-excursion
      (fsharp-run-process-if-needed)
      (dolist (file files)
        (fsharp-simple-send inferior-fsharp-buffer-name (concat "#load \"" file "\"")))
      ))
  (defun jsrn-fsharp-reload-project-entire ()
    "Reload ALL files of the project into the inferior buffer, including the
last main file"
    (interactive)
    (save-some-buffers)
    (jsrn-fsharp-load-files fsharp-ac-project-files)
    (fsharp-show-subshell)
    )
  (defun jsrn-fsharp-reload-project-libs ()
    "Reload all but the last file of the project into the inferior buffer"
    (interactive)
    (save-some-buffers)
    (jsrn-fsharp-load-files (butlast fsharp-ac-project-files))
    (fsharp-show-subshell)
  )
  (define-key fsharp-mode-map (kbd "C-c RET") 'fsharp-send-current-block)
  (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
  (define-key fsharp-mode-map (kbd "C-SPC") 'completion-at-point)
  (define-key fsharp-mode-map (kbd "C-c k") 'fsharp-goto-block-up)
  (define-key fsharp-mode-map [(f5)] 'jsrn-fsharp-reload-project-libs)
  (define-key fsharp-mode-map [(shift f5)] 'jsrn-fsharp-reload-project-entire)
  (define-key inferior-fsharp-mode-map (kbd "C-d")
    (lambda () (interactive) (evil-scroll-down 20)))
  )
(add-hook 'fsharp-mode-hook 'jsrn-fsharp-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       C/C++ AND GDB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c-add-style "jsrn"
             '("stroustrup"
               (c-offsets-alist
               )))

(defun jsrn-cc-mode-hook ()
  (interactive)
  (require 'xgtags "~/.emacs.d/xgtags.el")
  (xgtags-mode)
  ;;TODO: Make generic -- this sucks
  (setq xgtags-find-multiple-db (lambda (dir)
                                  (list "/home/jsrn/code/horrorville/trunk"
                                        "/usr/local/include/OGRE"
                                        "/usr/include/ois")))
  )
(add-hook 'c++-mode-hook 'jsrn-cc-mode-hook)

;; GDB for C/C++
(setq gdb-many-windows t)
(setq gdb-speedbar-auto-raise t)
(defun jsrn-gdb-mode-hook ()
  (interactive)
  (set-fringe-style 'default)
  (define-key evil-normal-state-local-map (kbd "C-p") 'gud-print)
  )
(add-hook 'gdb-frames-mode-hook 'jsrn-gdb-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ANKI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode anki-mode nil "anki"
  "Major mode writing Anki word lists"
  (setq tab-stop-list '(30 60))
  (setq-default indent-tabs-mode t)
  (load "beolingus")
  (load "sgml-mode")
  (defun anki-prepare ()
    "Clone this buffer, format it for anki importing it, and save it in homedir"
    (interactive)
    (let ((buf (current-buffer)))
      (with-temp-buffer
	(insert-buffer-substring buf)
	(goto-char (point-min))
	(while (re-search-forward " *\\(\t\\|   \\)[\t ]*" nil t)
	  (replace-match ";"))
	(write-file "~/anki_import.txt")
	)))
  ;; Some html bindings
  (fill-keymaps (list evil-visual-state-local-map
		      evil-insert-state-local-map)
		(kbd "C-M-b") (lambda () (interactive) (sgml-tag "b"))
		(kbd "C-<return>") (lambda () (interactive) (insert "<br/>"))
		(kbd "C-M-i")   (lambda () (interactive) (sgml-tag "i"))
		)
  )
(define-key anki-mode-map [(f2)] 'anki-prepare)
(define-key anki-mode-map [(f5)] (lambda () (interactive)
				    (beolingus-lookup (current-word))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SMTPMAIL AND MU4E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "mail_setup")
(add-hook 'administrative-mode-hook 'jsrn-mu4e-setup)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       MTG (MAGIC) LIST MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode mtg-list-mode nil "mtg"
  "Major mode writing MTG lists. Open motl list in one buffer and activate this
mode, and write in another also with this mode, then word completion works for
complete card names"
  (set (make-local-variable 'mtg-mode-variant) t)
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (modify-syntax-entry ?  "_" (syntax-table))
  (modify-syntax-entry ?\* " " (syntax-table))
  (modify-syntax-entry ?, "_" (syntax-table))
  (modify-syntax-entry ?' "_" (syntax-table))
  (modify-syntax-entry ?\( "_" (syntax-table))
  (modify-syntax-entry ?\) "_" (syntax-table))
  (modify-syntax-entry ?|  "." (syntax-table))
  (setq ac-sources (list 'ac-source-abbrev))
  (unless (string-match ".*motl.*" (buffer-name))
    (message "Remember to activate mtg-list-mode in MOTL buffer also"))
  ;;(define-key evil-insert-state-local-map (kbd "<return>") 'evil-ret)
  )
(setq mtg-counted-card-matcher "^[[:digit:]]* \\(.*\\)")
(defun jsrn-mtg-lookup-card-in-motl-buffer ()
  (interactive)
  (let* ((card-counted (current-word))
         (card (if (string-match mtg-counted-card-matcher card-counted)
                   (match-string 1 card-counted)
                 card-counted))
         (motl-buffer
          (find-first (buffer-list)
                      (lambda (buf)
                        (string-match ".*motl.*" (buffer-name buf))))))
    (pop-to-buffer motl-buffer)
    (goto-char (point-min))
    (search-forward card)
    ))
(defun jsrn-mtg-mode-hook ()
  (setq mtg-ac-source-counted-names (copy-list ac-source-words-in-same-mode-buffers))
  ;(add-to-list 'mtg-ac-source-counted-names '(prefix . mtg-counted-card-matcher))
  (setq ac-sources (list 'ac-source-words-in-same-mode-buffers 'mtg-ac-source-counted-names))
  (define-key mtg-list-mode-map [(f5)] 'jsrn-mtg-lookup-card-in-motl-buffer)
  (define-key mtg-list-mode-map (kbd "TAB") 'dabbrev-expand)
  (define-key mtg-list-mode-map (kbd "M-/") 'auto-complete)
  )
(add-hook 'mtg-list-mode-hook 'jsrn-mtg-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       TERMINAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq multi-term-program "/bin/zsh")
(defun jsrn-term-mode-hook ()
  (setq term-buffer-maximum-size 10000)
  ;; Define movement practical movement functions
  (defun term-on-prompt-line ()
    (interactive)
      (setq res 0)
      (save-excursion
        ;;there is a strange empty line after the prompt,so go 2 lines
        (setq res (> (forward-line 2) 0))
        )
      res
      )
  (defun term-goto-line-end ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-raw-string "\C-e")
      (evil-end-of-visual-line)))
  (defun term-goto-line-beginning ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-raw-string "\C-a")
      (evil-beginning-of-visual-line)))
  (defun term-forward-word ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-forward-word)
      (evil-forward-word-begin)))
  (defun term-backward-word ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-backward-word)
      (evil-backward-word-begin)))
  (defun term-goto-prompt-insert (append)
    (interactive)
    (evil-insert 0)
    (if append
        (term-send-right)
      (term-send-raw-string "a\C-?")
       ))
  (defun term-go-normal-left ()
    (interactive)
    (evil-backward-char)
    (when (term-on-prompt-line) (term-send-left)))
  (defun term-go-normal-right ()
    (interactive)
    (evil-forward-char)
    (when (term-on-prompt-line) (term-send-right)))
    ;; (unless (term-on-prompt-line)
    ;;   (term-send-raw "aC-?") ; an a and a backspace for resyncing position at prompt
    ;;   ))
    ;; (evil-insert 0) (term-send-right) (term-send-left))
  (defun term-send-prompt ()
    (interactive)
    (term-goto-prompt-insert nil)
    (term-goto-line-end)
    (sleep-for 0.1)
    (term-send-input)
    )
  (defun term-delete-line-back ()
    (interactive)
    (term-send-raw-string "\C-u"))
  (fill-keymap evil-normal-state-local-map
               (kbd evil-left-key) 'term-go-normal-left
               (kbd evil-right-key) 'term-go-normal-right
               (kbd "b") 'term-backward-word
               (kbd "w") 'term-forward-word
               (kbd "C-e") 'term-goto-line-end
               (kbd "C-a") 'term-goto-line-beginning
               (kbd "I")   '(lambda () (interactive) (term-goto-line-end) (evil-insert 0))
               (kbd "A")   '(lambda () (interactive) (term-goto-line-beginning) (evil-insert 0))
               (kbd "i")   '(lambda () (interactive) (term-goto-prompt-insert nil))
               (kbd "a")   '(lambda () (interactive) (term-goto-prompt-insert t))
               (kbd "D")   '(lambda () (interactive) (term-send-raw-string "\C-k"))
               (kbd "<return>")   'term-send-prompt
               ;TODO:
               ;  down-arrow
               ;  C-return for sending whatever is currently in front of cursor on line
               )
  (fill-keymaps (list evil-normal-state-local-map evil-insert-state-local-map)
                (kbd "C-<return>") 'term-send-prompt
                (kbd "M-<left>") 'multi-term-prev
                (kbd "M-<right>") 'multi-term-next
                (kbd "M-<backspace>")   'term-delete-line-back
                (kbd "C-<backspace>")   'term-send-backward-kill-word
                (kbd "C-r")         '(lambda () (interactive) (term-send-raw-string "\C-r"))
                )
  ;; NOTE: Remember that moving the cursor in Emacs does not (always) move the
  ;; cursor in term
  (fill-keymap evil-insert-state-local-map
               (kbd "<return>") 'term-send-input)
  (fill-keymap evil-visual-state-local-map
               (kbd "<return>") 'term-send-region)
  )

(add-hook 'term-mode-hook 'jsrn-term-mode-hook)
(defun last-term-buffer (l)
  "Return most recently used term buffer given a list of buffers."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))
(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))
(global-set-key [(f3)] 'get-term)


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
;;       MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(global-set-key [(f12)] 'magit-status)
(evil-set-initial-state 'magit-mode 'normal)
(fill-keymap magit-mode-map
	     (kbd "<return>") (lambda () (interactive) (magit-visit-item t))
	     (kbd "S-SPC")    'magit-show-item-or-scroll-down
             evil-down-key 'magit-goto-next-section
             evil-up-key   'magit-goto-previous-section
	     )
(defun jsrn-magit-mode-hook ()
  (interactive)
  (fill-keymap evil-motion-state-local-map
               (kbd (concat "C-" evil-down-key)) 'evil-next-line
               (kbd (concat "C-" evil-up-key))   'evil-previous-line
               ))
(add-hook 'magit-mode-hook 'jsrn-magit-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIMINISH (Cleaning up mode line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diminish)
(cl-loop for minor-mode in '(undo-tree-mode
                          auto-fill-function
                          visual-line-mode
                          highlight-parentheses-mode
                          flyspell-mode
                          reftex-mode)
      do (diminish minor-mode))
(add-hook 'LaTeX-mode-hook (lambda () (diminish 'outline-minor-mode)))
