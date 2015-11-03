(require 'ein)

(defalias 'ipython-notebook 'ein:notebooklist-open)

(setq ein:use-auto-complete t)

(fill-keymap ein:notebook-mode-map
             (kbd "C-<return>") 'ein:worksheet-execute-cell
             (kbd "M-{")        'ein:worksheet-goto-prev-input
             (kbd "M-}")        'ein:worksheet-goto-next-input
             )




(message "Loaded ipython_setup.el")
(provide 'ipython_setup)
