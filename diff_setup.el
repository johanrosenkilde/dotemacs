(defun jsrn-diff-mode-hook ()
  (interactive)
  (fill-keymap evil-motion-state-local-map
               evil-down-key 'diff-hunk-next
               evil-up-key   'diff-hunk-prev
               "q"           'kill-buffer)
  )
(add-hook 'diff-mode-hook 'jsrn-diff-mode-hook)
(jsrn-diff-mode-hook)

(message "Loaded diff_setup.el")
(provide 'diff_setup)
