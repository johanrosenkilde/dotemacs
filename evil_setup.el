;; Homerow movement keys
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


(setq-default evil-symbol-word-search t)
(setq evil-undo-system 'undo-tree
      evil-find-skip-newline t
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
(fill-keymaps (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map)
              [escape] 'minibuffer-keyboard-quit)
(fill-keymap isearch-mode-map [escape] 'isearch-cancel)
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
(setq evil-default-cursor #'cofi/evil-cursor)
 
;; Controlling windows
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

;; Redefine Vim-like movement to support Workman and my workflow
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
