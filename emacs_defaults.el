;; Global Emacs behaviour defaults

(setq initial-major-mode 'text-mode) ; set *scratch* buffer mode
(setq-default major-mode 'text-mode) ; set new buffers' major mode

(setq compilation-scroll-output t
      grep-find-command "grep -r --exclude=.git "  ;; grep ignores Git
      split-height-threshold 9999  ;; never automatically split horisontally
      sentence-end-double-space nil  ;; sentences end with a dot, not with two spaces
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))  ;; autosaves put away
      backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
      tab-width 4
      safe-local-variable-values nil
      column-number-mode t ;; activate column-number-mode globally

      ;; Use qutebrowser for hyperlinks
      browse-url-browser-function (lambda (url monkey) (call-process "/usr/local/bin/qutebrowser" nil 0 nil "--target" "auto" url))
      )
(setq-default fill-column 80
              indent-tabs-mode nil) ; never insert tabs, do spaces

;; Environment
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/johanrosenkilde/local/bin:/Users/johanrosenkilde/code/scripts"))

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
(add-to-list 'auto-mode-alist '("\\.sheet\\'" . sage-shell:sage-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-shell:sage-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; Build and keep list of recent files
(recentf-mode t)
(setq recentf-save-file "~/.emacs.d/.recentf")

;; Make the useless I into the very useful X with Ctrl and Meta
(keyboard-translate ?\C-i ?\C-x)
(define-key key-translation-map (kbd "M-i") (kbd "M-x"))
;; Do the same such that emacsclient understands
(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (with-selected-frame frame (keyboard-translate ?\C-i ?\C-x))
             (define-key key-translation-map (kbd "M-i") (kbd "M-x"))
             ))

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
(global-unset-key (kbd "C-x m")) ;; compose mail
