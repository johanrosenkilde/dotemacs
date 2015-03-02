(define-derived-mode anki-mode nil "anki"
  "Major mode for writing Anki word lists"
  (setq tab-stop-list '(30 60))
  (setq-default indent-tabs-mode t)
  ;(load "beolingus")
  (load "sgml-mode")
  (defun anki-prepare ()
    "Clone this buffer, format it for anki importing it, and save it in homedir"
    (interactive)
    (let ((buf (current-buffer)))
      (with-temp-buffer
	(insert-buffer-substring buf)
	(goto-char (point-min))
	(while (re-search-forward " *\\(\t\\|   \\)[\t ]*" nil t)
	  (replace-match ";"))
	(write-file "~/anki_import.txt")
	)))
  ;; Some html bindings
  (fill-keymaps (list evil-visual-state-local-map
		      evil-insert-state-local-map)
		(kbd "C-M-b") (lambda () (interactive) (sgml-tag "b"))
		(kbd "C-<return>") (lambda () (interactive) (insert "<br/>"))
		(kbd "C-M-i")   (lambda () (interactive) (sgml-tag "i"))
		)
  )
(define-key anki-mode-map [(f2)] 'anki-prepare)
(define-key anki-mode-map [(f5)] (lambda () (interactive)
				    (beolingus-lookup (current-word))))



(message "Loaded anki_setup.el")
(provide 'anki_setup)
