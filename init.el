;;; Speed up init
(load "speedup_init.el")

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
(global-set-key (kbd "S-<backspace>") 'delete-horizontal-space)
(global-set-key (kbd "M-<backspace>") 'kill-line-backwards)
(global-set-key (kbd "C-a") 'beginning-of-visual-line-smart) ;Override default C-a
(global-set-key [S-f1] 'show-clock)
(global-set-key (kbd "C-x C-=") '(lambda () (interactive) (modify-font-height 10)))
(global-set-key (kbd "C-x C--") '(lambda () (interactive) (modify-font-height -10)))
(global-set-key (kbd "C-x C-0") '(lambda () (interactive) (set-face-attribute 'default nil :height default-font-height)))
(global-set-key (kbd "C-x C-S-f") 'find-file-as-root)
(global-set-key [(f12)] 'magit-status)


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

;; Jump as an ace
(require 'ace-jump-mode)
(setq ace-jump-mode-scope 'window)

;; Load diminish (though diminish of usual suspects is done at the end)
(require 'diminish)

;; auto-complete setup
(global-company-mode)
(require 'pos-tip)
(define-key company-mode-map (kbd "C-/") 'hippie-expand)
(define-key company-mode-map (kbd "M-/") 'company-complete)

;; administrative mode and email
(setq administrative-mode-hook ())
(defun administrative-mode ()
  (interactive)
  (require 'administrative_setup "administrative_setup.el")
  (require 'mail_setup "mail_setup.el")
  (run-hooks 'administrative-mode-hook))

;; Evil
(load "evil_setup.el")

;; Ido
(require 'ido_setup "ido_setup.el")

;; Surround mode
;; (require 'surround_setup "surround_setup.el")

;; Dired
(require 'dired_setup)

;; Flyspell
(load "flyspell_setup.el")

;; hideshow minor mode
(load "hideshow_setup.el")

;; desktop (session management)
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

;; Undo-tree mode
(require 'undo-tree)
(global-undo-tree-mode)
(define-key undo-tree-visualizer-mode-map (kbd "n") 'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-mode-map (kbd "e") 'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map (kbd "y") 'undo-tree-visualize-switch-branch-left)
(define-key undo-tree-visualizer-mode-map (kbd "o") 'undo-tree-visualize-switch-branch-right)
(define-key undo-tree-map (kbd "C-/") nil)

;; Evil-numbers (Vim-like increment and decrement)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Cheat.sh lookup
(require 'cheat-sh)
(global-set-key (kbd "C-h q") 'cheat-sh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       PREPARATION OF OPTIONALLY LOADED MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook (lambda () (visual-line-mode) (auto-fill-mode)))

(defun setup-cpp ()
  (require 'cpp_setup "cpp_setup.el"))
(add-hook 'c++-mode-hook 'setup-cpp)

(defun setup-org ()
  (require 'org_setup "org_setup.el"))
(add-hook 'org-mode-hook 'setup-org)

(defun setup-latex ()
  (require 'latex_setup "latex_setup.el"))
(add-hook 'LaTeX-mode-hook 'setup-latex)

(defun jsrn-compilation-mode-hook ()
  (local-unset-key "g") ;; disable "recompile" command to reinstate Evil's g
  )
(add-hook 'compilation-mode-hook 'jsrn-compilation-mode-hook)

(defun setup-lisp ()
  (require 'lisp_setup "lisp_setup.el"))
(add-hook 'emacs-lisp-mode-hook 'setup-lisp)

(defun setup-python ()
  (require 'python_setup "python_setup.el"))
(add-hook 'python-mode-hook 'setup-python)

(autoload 'sage-shell:sage-mode "sage_setup.el" "Major mode for Sage" t)
(autoload 'ipython-notebook "ipython_setup.el" "Browse and open an IPython notebook" t)
(autoload 'tuareg-mode "ocaml_setup.el" "Major mode for Ocaml" t)
(autoload 'rust-mode "rust_setup.el" "Major mode for Rust" t)
(autoload 'haskell-mode "haskell_setup.el" "Major mode for Haskell" t)
(autoload 'fsharp-mode "fsharp_setup.el" "Major mode for F-sharp" t)
(autoload 'diff-mode "diff_setup.el" "Major mode for diff" t)
(autoload 'web-mode "web_setup.el" "Major mode for web (html, php, etc.)" t)
(autoload 'magit-status "magit_setup.el" "Git repository status using Magit" t )
(autoload 'monky-status "monky_setup.el" "Mercurial repository status using Monky" t )
(autoload 'ledger-mode "ledger_setup.el" "Load ledger setup")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       DIMINISH (Cleaning up mode line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-loop for minor-mode in '(undo-tree-mode
                          auto-fill-function
                          visual-line-mode
                          highlight-parentheses-mode
                          ivy-minor-mode
                          company-minor-mode
                          eldoc-mode
                          auto-revert-mode
                          company-mode
                          flycheck-mode)
      do (diminish minor-mode))
