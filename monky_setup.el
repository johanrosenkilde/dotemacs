(require 'monky)
(evil-set-initial-state 'monky-mode 'emacs)
(fill-keymap monky-mode-map
	     (kbd "<return>") (lambda () (interactive) (monky-visit-item t))
	     (kbd "S-SPC")    'monky-show-item-or-scroll-down
             evil-down-key 'monky-goto-next-section
             evil-up-key   'monky-goto-previous-section
	     )
(defun jsrn-monky-mode-hook ()
  (interactive)
  (fill-keymap evil-motion-state-local-map
               (kbd (concat "C-" evil-down-key)) 'evil-next-line
               (kbd (concat "C-" evil-up-key))   'evil-previous-line
               ))
(add-hook 'monky-mode-hook 'jsrn-monky-mode-hook)



(message "Loaded monky_setup.el")
(provide 'monky_setup)
