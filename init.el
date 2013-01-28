(server-start)
(set-face-attribute 'default nil :height 70)
(setq inhibit-splash-screen t)
(tool-bar-mode 0)

;; Other global nice options
(toggle-scroll-bar -1) ;; Emacs gurus don't need no stinking scroll bars
(set-fringe-mode '(0 . 1)) ;activate only the right fringe area
(setq compilation-scroll-output t)
(setq-default indent-tabs-mode nil)
(setq mouse-drag-copy-region t) ;; mouse region copies
(setq grep-find-command "grep -r --exclude=.git ") ;; grep ignores Git

;; File type default modes
(add-to-list 'auto-mode-alist '("\\.svg\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.scene\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))


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
      (decf n)
      (push (car lst) acc)
      (setq lst (cdr lst)))
    (nreverse acc)))

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

(global-set-key [(f1)] '(lambda ()
                          (interactive)
                          (manual-entry (current-word))))
(global-set-key [(f2)] '(lambda ()
                          (interactive)
                          (save-buffer)
                          (if (fboundp 'recompile)
                              (recompile)
                            (compile))))
(global-set-key [(f5)] 'orgtbl-mode)
(global-set-key [(shift f5)] 'orgtbl-insert-radio-table)
(global-set-key "\M-?" 'hippie-expand)
;; Other window control
(global-set-key (kbd "C-M-b") 'scroll-other-window-down)
(global-set-key (kbd "C-M-d") 'scroll-other-window)

(defun kill-line-backwards ()
  "Kill the current line backwards from the current column.

Kill the current line backwards from the current column. If at col 0, kill
only the newline character"
  (interactive)
  (if (= (current-column) 0) ; If we are at beginning, kill newline char
      (backward-delete-char 1)
    (kill-line 0)))
(global-set-key (kbd "M-C-<backspace>") 'kill-line-backwards)

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

;;Function for reloading the .emacs file
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
;; Fill sentence: reflows paragraph to have only linebreaks at sentence boundaries
(load "fill-sentence.el")

;; minor mode Highlight parentheses which are around cursor
(require 'highlight-parentheses)

;; Global loading
;; Load ido -- alternatives shown directly in minibuffer + more
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;match substr on what is written

;; Uniquify gives better names to buffers containing files with same base name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Winner gives undo and redo of windows arrangements
(require 'winner)
(winner-mode 1)
(global-set-key (kbd "M-<left>") 'winner-undo)
(global-set-key (kbd "M-<right>") 'winner-redo)


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
(fill-keymaps (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map)
              [escape] 'minibuffer-keyboard-quit)
;; Key-bindings in all modes
(fill-keymaps (list evil-normal-state-map
                    evil-visual-state-map
                    evil-insert-state-map)
              ;; TAB is overtaken by Emacs (which is ok), so map jump-forward to C-Esc
              (kbd "C-<escape>") 'evil-jump-forward
              (kbd "C-e")        'end-of-visual-line
              (kbd "C-a")        'beginning-of-visual-line-smart
              (kbd "C-b")        'evil-scroll-up)
;; Key-bindings in normal mode
(fill-keymap evil-normal-state-map
             (kbd "S-a") '(lambda () (interactive) (end-of-visual-line) (evil-insert-state))
             ;; Search using Emacs' isearch but using Vim keybindings
             "/" 'isearch-forward
             "?" 'isearch-backward
             "n" 'isearch-repeat-forward
             "N" 'isearch-repeat-backward
             ;; Tab in normal mode works as tab in Emacs
             (kbd "TAB") 'indent-for-tab-command)
;; Key-bindings in insert mode
(fill-keymap evil-insert-state-map
             (kbd "C-y") 'yank)
;; Key-bindings in visual mode
(fill-keymap evil-visual-state-map
             ;; Provide a visual-time shorcut to commenting
             "z" 'comment-region
             "Z" 'uncomment-region)
;; Key-bindings for movement
(fill-keymap evil-motion-state-map
             "j" 'evil-next-visual-line
             "k" 'evil-previous-visual-line
             "$" 'evil-end-of-visual-line
             "^" 'evil-first-non-blank-of-visual-line)

; Disable Evil in certain modes
(loop for (mode . state) in '((eassist-mode . emacs)
                              (xgtags-select-mode . emacs)
                              (magit-branch-manager-mode . emacs))
      do (evil-set-initial-state mode state))

(evil-mode 1)

; In some modes, the special Enter is most I need, so map this to C-<enter>
(loop for (mode . fun) in '((man-mode . man-follow))
      do (evil-declare-key normal mode "C-<enter>" 'fun))

;; Emulate surround.vim
;; Usage description really quick:
;; type c s <delimiter> <replacement> for replacing the nearest <delimiter> pair
;; with appropriately chosen <replacement> pair. Use left delimiter to get space
;; on insider, and right delimiter otherwise
;; type d s <delimiter> to remove delimiter pair
;; In visual mode, type s <new delim> to insert delimiter (same rules w. spaces)
;;                 type S <new delim> to insert also newlines on inside
(require 'surround)
(surround-mode 1)
;; Some extras for certain modes
(add-hook 'LaTeX-mode-hook (lambda ()
                             (push '(?~ . ("\\texttt{" . "}")) surround-pairs-alist)
                             (push '(?/ . ("\\emph{"   . "}")) surround-pairs-alist)
                             (push '(?* . ("\\textbf{" . "}")) surround-pairs-alist)))

;; make cursor look like Vim when in Vim normal moe
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

(eval-after-load 'man
  '(progn
     ;; use Evil but add some keys
     (evil-make-overriding-map evil-normal-state-map 'normal t)
     (evil-define-key 'normal
       "C-<enter>" 'man-follow nil))) ;the nil is needed for only one define; it's a bug in Evil

(eval-after-load 'dired
  '(progn
     ;; use the standard Dired bindings as a base
     (evil-make-overriding-map dired-mode-map 'normal t)
     (evil-define-key 'normal
       "J" 'dired-goto-file ; was "j"
       "K" 'dired-do-kill-lines ; was "k"
       "r" 'dired-do-redisplay))) ; was "l"

;; windowing
(fill-keymap evil-window-map
             (kbd "C-g") nil
             ;; Splitting
             "\\" 'split-window-vertically
             "|"  'split-window-horizontally
             ;; Deleting
             "D" 'delete-window
             (kbd "C-d") 'delete-window
             "1" 'delete-other-windows
             ;; Sizing
             (kbd "RET") 'enlarge-window)

;; Put selected Evil Keys in Emacs mode
(fill-keymap evil-emacs-state-map
             (kbd "C-w h") 'evil-window-left
             (kbd "C-w j") 'evil-window-down
             (kbd "C-w k") 'evil-window-up
             (kbd "C-w l") 'evil-window-right
             (kbd "C-w C-w") 'evil-window-prev)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-startup-indented t)
(defun jsrn-org-mode-hook ()
  (visual-line-mode t)
  (define-key evil-normal-state-map (kbd "M-l") 'org-metaright)
  (define-key evil-insert-state-map (kbd "M-l") 'org-metaright)
  (define-key evil-insert-state-map (kbd "M-h") 'org-metaleft)
  (define-key evil-normal-state-map (kbd "M-h") 'org-metaleft)
  (define-key evil-normal-state-map (kbd "M-k") 'org-metaup)
  (define-key evil-normal-state-map (kbd "M-j") 'org-metadown)
  (define-key evil-normal-state-map (kbd "M-K") 'org-shiftmetaup)
  (define-key evil-normal-state-map (kbd "M-J") 'org-shiftmetadown)
  ;; Let winner keys overwrite org-mode
  (define-key evil-normal-state-map (kbd "M-<left>") 'winner-undo) 
  (define-key evil-normal-state-map (kbd "M-<right>") 'winner-redo)
  )
(add-hook 'org-mode-hook 'jsrn-org-mode-hook)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       LATEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-parse-self nil)

(defun jsrn-latex-mode-hook ()
  (local-set-key (kbd "M-q") 'fill-sentence)  ; hard sentence wrap
  (local-set-key [(f3)] 'TeX-view)   ; possibly open xdvi and goto line
  (setq fill-column 9999)            ; with hard senctence wrap, we don't want hard lines
  (visual-line-mode t)               ; but we do want visual word wrap
  (adaptive-wrap-prefix-mode t)      ; with adaptive indenting
  (setq LaTeX-item-indent 0)         ; indent \item as other stuff inside envs (works
                                        ; better with adaptive-wrap-prefix-mode)
  (LaTeX-math-mode)                  ; always turn on math mode
  (setq TeX-insert-braces nil)       ; dont ever insert braces at macro expansion
  (setq TeX-source-correlate-method 'source-specials)  ;; auctex 10.86  
   (TeX-source-correlate-mode)
  ;; Teach AucTeX about IEEEeqnarray
  (LaTeX-add-environments
   '("IEEEeqnarray" LaTeX-env-label)
   '("IEEEeqnarray*" LaTeX-env-label))
  (add-to-list 'font-latex-math-environments "IEEEeqnarray")
  (add-to-list 'font-latex-math-environments "IEEEeqnarray*")
  (setq texmathp-tex-commands (("IEEEeqnarray" env-on) ("IEEEeqnarray*" env-on)))
  )
(add-hook 'LaTeX-mode-hook 'jsrn-latex-mode-hook)


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
  ;; Change dired-up-directory to find-alternate-file ..
  (lambda () (define-key dired-mode-map (kbd "^")
               (lambda () (interactive) (find-alternate-file ".."))))
  )
(add-hook 'dired-mode-hook 'jsrn-dired-mode-hook)
;; Load the advanced, not-touched-so-often stuff
(load "dired_setup")



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
(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(defun jsrn-spell-goto-next-and-suggest ()
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(define-key flyspell-mode-map "C-," 'jsrn-spell-goto-next-and-suggest)

(setq ispell-silently-savep t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [(f12)] 'magit-status)
(evil-set-initial-state 'magit-mode 'normal)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DESKTOP (session management)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'desktop)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-base-file-name "desktop")
(setq jsrn-desktop-conf-file-name "windows")
(setq desktop-save t) ; don't ask, just act
(setq jsrn-desktop-base-dir "~/.emacs.d/desktops/")
(setq jsrn-desktop-current nil)
(defun desktop-save-new (desktop)
  "Save the current desktop as a new desktop"
  (interactive "sName of desktop: ")
  (setq jsrn-desktop-current desktop)
  (let ((dirname (concat jsrn-desktop-base-dir jsrn-desktop-current)))
    (mkdir dirname t)
    (desktop-save dirname t)))

(defun desktop-discard ()
  "Discard the current desktop without saving and clear everything"
  (interactive)
  (if (y-or-n-p "Are you sure you wish to discard the current desktop without saving?")
    ((setq jsrn-desktop-current nil)
     (desktop-clear))))

(defun desktop-put-away-current-for-switch ()
  "Save the current desktop and clears as preparation for a desktop switch.
   Usually not necessary to call directly"
  (interactive)
  (if (eq jsrn-desktop-current nil)
      (if (y-or-n-p "Do you wish to save your current unnamed desktop first?")
          (call-interactively 'desktop-save-new))
    (desktop-save (concat jsrn-desktop-base-dir jsrn-desktop-current) t))
  (desktop-clear))

(defun desktop-create-new (desktop)
  "Create a new, blank desktop. Saves the current desktop first"
  (interactive "sName of desktop: ")
  (desktop-put-away-current-for-switch)
  (setq desktop-dirname (concat jsrn-desktop-base-dir jsrn-desktop-current))
  (desktop-save-new desktop))

(defun desktop-switch (desktop)
  (interactive (list (completing-read "Switch to desktop: "
                                      (directory-files jsrn-desktop-base-dir))))
  (desktop-put-away-current-for-switch)
  (setq jsrn-desktop-current desktop)
  (desktop-read (concat jsrn-desktop-base-dir jsrn-desktop-current)))

(defun desktop-save-on-kill-emacs ()
  "Save the current desktop, if set, when emacs dies. Never query the user"
  (interactive)
  (if (not (eq jsrn-desktop-current nil))
      (desktop-save (concat jsrn-desktop-base-dir jsrn-desktop-current) t)))
(add-hook 'kill-emacs-hook 'desktop-save-on-kill-emacs)


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
  (define-key evil-normal-state-map (kbd "C-p") 'gud-print)
  )
(add-hook 'gdb-frames-mode-hook 'jsrn-gdb-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIMINISH (Cleaning up mode line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'auto-fill-function)
(diminish 'visual-line-mode)
(diminish 'highlight-parentheses-mode)
