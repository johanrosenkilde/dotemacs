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

(message "Loaded lisp_setup.el")
(provide 'lisp_setup)
