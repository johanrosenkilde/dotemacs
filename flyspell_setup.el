(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(setq ispell-dictionary "british")
;; Cycle through dictionaries. First make the language ring
(let ((langs '("dansk" "british")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun jsrn-cycle-dictionary ()
  (interactive)
  (let* ((cur (if (or (not (boundp 'ispell-local-dictionary)) (eq nil ispell-local-dictionary))
                 (ring-ref lang-ring -1)
               ispell-local-dictionary))
         (new (ring-next lang-ring cur)))
    (progn
      (ispell-change-dictionary new)
      (message "Changed dictionary to %s (was %s)" new cur)
      )))
(defun jsrn-spell-goto-next-and-suggest ()
  (interactive)
  (evil-set-jump)
  (flyspell-goto-next-error)
  (ispell-word))
(setq flyspell-auto-correct-binding nil
      flyspell-highlight-flag t
      )
(define-key flyspell-mode-map [(control ?\.)] 'jsrn-spell-goto-next-and-suggest)
(define-key flyspell-mode-map [(control ?\,)] nil)
(define-key flyspell-mode-map [(f6)] 'jsrn-cycle-dictionary)

(setq ispell-silently-savep t)


(message "Loaded flyspell_setup.el")
(provide 'flyspell_setup)
