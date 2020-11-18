;; Web mode (HTML, PHP)
(defun jsrn-web-mode-hook ()
  (define-key web-mode-map (kbd "C-c .") 'web-mode-mark-and-expand)
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook 'jsrn-web-mode-hook)

(message "Loaded web_setup.el")
(provide 'web_setup)
