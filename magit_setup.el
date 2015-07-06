(require 'magit)
(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-process-mode 'emacs)
(fill-keymap magit-mode-map
	     (kbd "<return>") (lambda () (interactive) (magit-visit-item t))
	     (kbd "S-SPC")    'magit-show-item-or-scroll-down
	     )
(defun jsrn-magit-mode-hook ()
  (interactive)
  (visual-line-mode)
  (fill-keymap evil-motion-state-local-map
               (kbd (concat "C-" evil-down-key)) 'evil-next-line
               (kbd (concat "C-" evil-up-key))   'evil-previous-line
               )
  (fill-keymap magit-mode-map
               evil-up-key     'magit-section-up
               evil-down-key   'magit-section-forward
               evil-up-key-uc    'previous-line
               evil-down-key-uc  'next-line
    ))
(add-hook 'magit-mode-hook 'jsrn-magit-mode-hook)



(message "Loaded magit_setup.el")
(provide 'magit_setup)
