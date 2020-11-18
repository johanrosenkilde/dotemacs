;;; Speed up init
(load "speedup_init.el")

;; Keyboard layout to expect
(setq workman t)

;; About me
(setq user-full-name "Johan S. H. Rosenkilde"
      jsrn-user-mail-address "jsrn@jsrn.dk") ;; Std email; I will overwrite user-mail-address

;; Setup the interface
(load "interface_setup.el")

;; Load global defaults for Emacs behaviour
(load "emacs_defaults.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PACKAGE MANAGER / MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(when (< emacs-major-version 27)
  (package-initialize))

(require 'use-package)

(load "elisp_utils.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       GLOBALLY DEFINED CUSTOM FUNCTIONS AND KEYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "helper_functions.el")

(global-set-key [(f2)] 'jsrn-recompile)
(global-set-key [(f3)] nil)
(global-set-key [(f4)] 'ffap) ;; look-up file at point
(global-set-key (kbd "M-?") 'hippie-expand)
(global-set-key (kbd "C-x m") 'ffap) ;; Find file at point (and override compose-mail)
(global-set-key (kbd "S-<backspace>") 'delete-horizontal-space)
(global-set-key (kbd "M-<backspace>") 'kill-line-backwards)
(global-set-key (kbd "C-a") 'beginning-of-visual-line-smart) ;Override default C-a
(global-set-key [S-f1] 'show-clock)
(global-set-key (kbd "C-x C-=") '(lambda () (interactive) (modify-font-height 10)))
(global-set-key (kbd "C-x C--") '(lambda () (interactive) (modify-font-height -10)))
(global-set-key (kbd "C-x C-0") '(lambda () (interactive) (set-face-attribute 'default nil :height default-font-height)))


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


;; Multiple cursors
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       AUTO-COMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-company-mode)
(require 'pos-tip)

(define-key company-mode-map (kbd "C-/") 'hippie-expand)
(define-key company-mode-map (kbd "M-/") 'company-complete)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       QUICK-EDIT INTEGRATION (e.g. Qutebrowser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun quickedit (file line column)
  (find-file file)
  (goto-line line)
  (beginning-of-line)
  (forward-char (- column 1))
  (message "Quick-edit: Press C-c C-c when you're done")
  (local-set-key (kbd "C-c C-c")
                 (lambda ()
                   (interactive)
                   (save-buffer)
                   (kill-buffer)
                   (delete-frame))))


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
  (evil-global-set-key 'normal (kbd "C-!") 'secret-lookup)
  (evil-global-set-key 'emacs (kbd "C-!") 'secret-lookup)
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

(setq-default evil-symbol-word-search t)
(setq evil-find-skip-newline t
      evil-move-cursor-back nil
      evil-ex-search-highlight-all t
      lazy-highlight-cleanup nil
      evil-want-fine-undo t
      evil-want-abbrev-expand-on-insert-exit nil
      )

(setq evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

(require 'evil)

;; Jump like in the good ol' Evil days
(require 'jsrn_jumps)

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
;; (global-set-key [escape] 'evil-exit-emacs-state)
(fill-keymap evil-emacs-state-map
             [escape] 'keyboard-quit)


;; Key-bindings in all modes
(fill-keymaps (list evil-normal-state-map
                    evil-visual-state-map
                    evil-insert-state-map)
              ;; TAB is overtaken by Emacs (which is ok), so map jump-forward to C-Esc
              (kbd "C-<escape>") 'jsrn-evil-jump-forward
              (kbd "C-o")        'jsrn-evil-jump-backward ;; override new Evil jumper
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
             (kbd "C-#") 'jsrn-goto-first-symbol-use
             (kbd "M-p") (lambda () (interactive) (evil-paste-pop -1))
             ;; Tab in normal mode works as tab in Emacs
             (kbd "TAB") 'indent-for-tab-command
             (kbd "C-y") 'yank
             (kbd "M-,") 'ido-goto-symbol
             (kbd "z d") (lambda () (interactive) (kill-buffer (current-buffer))) ;; kill current buffer
             )
;; Key-bindings in insert mode
(fill-keymap evil-insert-state-map
             (kbd "<return>") 'newline-and-indent
             (kbd "C-y") 'yank
             ;; (kbd "C-p") 'evil-complete-previous
             ;; (kbd "C-k") 'evil-complete-previous
             )
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
  (jsrn-evil-set-jump))
(defadvice isearch-repeat-forward (before marker activate)
  "Store current position in jump list"
  (jsrn-evil-set-jump))
(defadvice isearch-backward (before marker activate)
  "Store current position in jump list"
  (jsrn-evil-set-jump))
(defadvice isearch-repeat-backward (before marker activate)
  "Store current position in jump list"
  (jsrn-evil-set-jump))

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
                              (Buffer-menu-mode . normal)
                              (help-mode . normal)
                              (apropos-mode . normal)
                              (Info-mode . normal)
                              (woman-mode . normal)
                              (compilation-mode . normal)
                              (git-commit-mode . normal)
                              )
      do (evil-set-initial-state mode state))

;; Remove some key bindings which seem to take precedence over Evil
(define-key Buffer-menu-mode-map "e" nil)

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
(evil-declare-key 'insert sage-shell-mode-map (kbd "<return>") 'sage-shell:send-input)
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
  (evil-scroll-down 0)
  (recenter)
  )
(evil-define-motion jsrn-scroll-up ()
  "Scroll up half a page and recenter"
  :type inclusive
  (evil-scroll-up 0)
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

;; Redefine evil-visual-update-x-selection so that visuals are copied to X's
;; PRIMARY cliboard
;; see https://bitbucket.org/lyro/evil/issues/532/evil-uses-the-clipboard-instead-of-the
(defun evil-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region."
  (with-current-buffer (or buffer (current-buffer))
    (when (and (evil-visual-state-p)
               (fboundp 'x-set-selection)
               (or (not (boundp 'ns-initialized))
                   (with-no-warnings ns-initialized))
               (not (eq evil-visual-selection 'block)))
      (let ((text (buffer-substring-no-properties
                                 evil-visual-beginning
                                 evil-visual-end)))
        (x-set-selection 'PRIMARY text)
        (setq x-last-selected-text-primary text)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       LOAD GLOBAL SETUP OF SPECIAL MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido_setup "ido_setup.el")
;; (require 'surround_setup "surround_setup.el")
(require 'dired_setup)

;; TODO: Some package that is autoloaded at startup apparently loads flyspell.
;; This shouldn't happen
(load "flyspell_setup.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       HIDESHOW MINOR MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hideshow)
(setq hs-isearch-open t) ; search in both code and comments
(setq py-hide-show-minor-mode-p t) 

;; Highlight folded regions better, and add number of lines folded
(defun hs-overlay-style (ov)
  (overlay-put ov 'face '(background-color . "#ffdfd2"))
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
                 (propertize
                  (format " ... <%d>"
                          (count-lines (overlay-start ov)
                                       (overlay-end ov)))
                  'face 'font-lock-type-face))))
(setq hs-set-up-overlay 'hs-overlay-style)

(setq jsrn-hs-hiding-all nil)
(defun hs-toggle-all ()
  (interactive)
  (if (not (local-variable-p jsrn-hs-hiding-all))
      (defvar-local jsrn-hs-hiding-all nil))
  (if jsrn-hs-hiding-all
      (progn
        (setq jsrn-hs-hiding-all nil)
        (hs-show-all))
    (progn
        (setq jsrn-hs-hiding-all t)
        (hs-hide-all)))
  )
(fill-keymap hs-minor-mode-map
             (kbd "C-<tab>" ) 'hs-toggle-hiding
             (kbd "M-C-<tab>" ) 'hs-toggle-all
             )
      




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
  ;; (require 'paredit)
  ;; (paredit-mode)
  (fill-keymap evil-normal-state-local-map
    "s" 'forward-sexp
    "S" 'backward-sexp
    "Q" (lambda () (interactive) (up-list -1))
    )
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
  (define-key lisp-mode-shared-map (kbd "C-c C-h") 'jsj-ac-show-help)
)
(add-hook 'emacs-lisp-mode-hook 'jsrn-emacs-lisp-mode-hook)

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
; the following alias is to avoid escaping hyphens in i3's config file
(defun jsrn-switch-to-mat ()
  (mdesktop-switch-noninteractive "mat"))


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
(autoload 'sage-shell:sage-mode "sage_setup.el" "Major mode for Sage" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       IPYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ipython-notebook "ipython_setup.el" "Browse and open an IPython notebook" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       OCAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'tuareg-mode "ocaml_setup.el" "Major mode for Ocaml" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       RUST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'rust-mode "rust_setup.el" "Major mode for Rust" t)

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
(add-hook 'text-mode-hook (lambda ()
                            (visual-line-mode)
                            (auto-fill-mode)))

;; Undo-tree mode
(require 'undo-tree)
(global-undo-tree-mode)
(define-key undo-tree-visualizer-mode-map (kbd "n") 'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-mode-map (kbd "e") 'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map (kbd "y") 'undo-tree-visualize-switch-branch-left)
(define-key undo-tree-visualizer-mode-map (kbd "o") 'undo-tree-visualize-switch-branch-right)
(define-key undo-tree-map (kbd "C-/") nil)

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
;; Printing only rarely used - no need to enable it
;; (require 'printing)		; load printing package
;; (setq jsrn-pr-printer-alist
;;       '((et2-006 "lpr"     nil "et2-color-konica-006")   ;; Ulm University
;;         ))
;; (setq jsrn-current-printer 'et2-006)
;; (defun jsrn-pr-set-printers ()
;;   (interactive)
;;   (setq pr-path-alist
;;         '((unix      "." ghostview mpage PATH)
;;           (ghostview "/usr/bin/gv")
;;           (mpage     "/usr/bin/mpage")
;;           ))
;;   (setq pr-txt-printer-alist jsrn-pr-printer-alist)
;;   (setq pr-ps-printer-alist jsrn-pr-printer-alist)
;;   (setq pr-txt-name  jsrn-current-printer)
;;   (setq pr-ps-name  jsrn-current-printer)
;;   (pr-update-menus t)		; update now printer and utility menus
;; )
;; (jsrn-pr-set-printers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       CHEAT.SH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cheat-sh)
(global-set-key (kbd "C-h q") 'cheat-sh)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIMINISH USUAL SUSPECTS (Cleaning up mode line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-loop for minor-mode in '(undo-tree-mode
                          auto-fill-function
                          visual-line-mode
                          highlight-parentheses-mode
                          ivy-minor-mode
                          company-minor-mode
                          eldoc-mode
                          auto-revert-mode
                          flycheck-mode)
      do (diminish minor-mode))
