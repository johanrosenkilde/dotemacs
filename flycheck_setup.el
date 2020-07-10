(require 'flycheck)

(fill-keymap flycheck-mode-map
             (kbd "C-M-n")  'flycheck-next-error
             (kbd "C-M-p")  'flycheck-previous-error
             )

(message "Loaded flycheck_setup.el")
(provide 'flycheck_setup)
;;; flycheck_setup ends here
