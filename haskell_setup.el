(defun jsrn-haskell-mode-hook ()
  ;; (turn-on-haskell-doc-mode "haskell-doc" nil t)
  (turn-on-haskell-indentation)
  (setq haskell-process-type 'cabal-repl)
  (interactive-haskell-mode)
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  )
(add-hook 'haskell-mode-hook 'jsrn-haskell-mode-hook)

(jsrn-haskell-mode-hook)

(message "Loaded haskell_setup.el")
(provide 'haskell_setup)
