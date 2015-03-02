(require 'magit)
(global-set-key [(f12)] 'magit-status)
(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-process-mode 'emacs)
(fill-keymap magit-mode-map
	     (kbd "<return>") (lambda () (interactive) (magit-visit-item t))
	     (kbd "S-SPC")    'magit-show-item-or-scroll-down
             evil-down-key 'magit-goto-next-section
             evil-up-key   'magit-goto-previous-section
	     )
(defun jsrn-magit-mode-hook ()
  (interactive)
  (visual-line-mode)
  (fill-keymap evil-motion-state-local-map
               (kbd (concat "C-" evil-down-key)) 'evil-next-line
               (kbd (concat "C-" evil-up-key))   'evil-previous-line
               ))
(add-hook 'magit-mode-hook 'jsrn-magit-mode-hook)



(message "Loaded magit_setup.el")
(provide 'magit_setup)
