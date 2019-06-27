(require 'magit)
(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-process-mode 'emacs)
(fill-keymap magit-mode-map
	     (kbd "S-SPC")    'magit-show-item-or-scroll-down
	     )
(setq git-commit-summary-max-length 100)
(setq magit-restore-window-configuration nil) ;; don't attempt stupid restore of look before magit
(setq magit-diff-refine-hunk 'all)
(setq magit-diff-highlight-hunk-body nil)
;; TODO: add toggle-truncate-line to disable it?
(defun jsrn-magit-mode-hook ()
  (interactive)
  (visual-line-mode)
  (fill-keymap evil-motion-state-local-map
               (kbd (concat "C-" evil-down-key)) 'evil-next-line
               (kbd (concat "C-" evil-up-key))   'evil-previous-line
               )
  (fill-keymap magit-mode-map
               evil-up-key     'magit-section-backward
               evil-down-key   'magit-section-forward
               evil-up-key-uc    'previous-line
               evil-down-key-uc  'next-line
               )
  (fill-keymap magit-revision-mode-map
               (kbd "C-RET") 'magit-diff-visit-file-worktree
               )
  )
(add-hook 'magit-mode-hook 'jsrn-magit-mode-hook)



(message "Loaded magit_setup.el")
(provide 'magit_setup)
