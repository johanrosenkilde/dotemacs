(setq multi-term-program "/bin/zsh")
(defun jsrn-term-mode-hook ()
  (setq term-buffer-maximum-size 10000)
  ;; Define movement practical movement functions
  (defun term-on-prompt-line ()
    (interactive)
      (setq res 0)
      (save-excursion
        ;;there is a strange empty line after the prompt,so go 2 lines
        (setq res (> (forward-line 2) 0))
        )
      res
      )
  (defun term-goto-line-end ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-raw-string "\C-e")
      (evil-end-of-visual-line)))
  (defun term-goto-line-beginning ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-raw-string "\C-a")
      (evil-beginning-of-visual-line)))
  (defun term-forward-word ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-forward-word)
      (evil-forward-word-begin)))
  (defun term-backward-word ()
    (interactive)
    (if (term-on-prompt-line)
        (term-send-backward-word)
      (evil-backward-word-begin)))
  (defun term-goto-prompt-insert (append)
    (interactive)
    (evil-insert 0)
    (if append
        (term-send-right)
      (term-send-raw-string "a\C-?")
       ))
  (defun term-go-normal-left ()
    (interactive)
    (evil-backward-char)
    (when (term-on-prompt-line) (term-send-left)))
  (defun term-go-normal-right ()
    (interactive)
    (evil-forward-char)
    (when (term-on-prompt-line) (term-send-right)))
    ;; (unless (term-on-prompt-line)
    ;;   (term-send-raw "aC-?") ; an a and a backspace for resyncing position at prompt
    ;;   ))
    ;; (evil-insert 0) (term-send-right) (term-send-left))
  (defun term-send-prompt ()
    (interactive)
    (term-goto-prompt-insert nil)
    (term-goto-line-end)
    (sleep-for 0.1)
    (term-send-input)
    )
  (defun term-delete-line-back ()
    (interactive)
    (term-send-raw-string "\C-u"))
  (fill-keymap evil-normal-state-local-map
               (kbd evil-left-key) 'term-go-normal-left
               (kbd evil-right-key) 'term-go-normal-right
               (kbd "b") 'term-backward-word
               (kbd "w") 'term-forward-word
               (kbd "C-e") 'term-goto-line-end
               (kbd "C-a") 'term-goto-line-beginning
               (kbd "I")   '(lambda () (interactive) (term-goto-line-end) (evil-insert 0))
               (kbd "A")   '(lambda () (interactive) (term-goto-line-beginning) (evil-insert 0))
               (kbd "i")   '(lambda () (interactive) (term-goto-prompt-insert nil))
               (kbd "a")   '(lambda () (interactive) (term-goto-prompt-insert t))
               (kbd "D")   '(lambda () (interactive) (term-send-raw-string "\C-k"))
               (kbd "<return>")   'term-send-prompt
               ;TODO:
               ;  down-arrow
               ;  C-return for sending whatever is currently in front of cursor on line
               )
  (fill-keymaps (list evil-normal-state-local-map evil-insert-state-local-map)
                (kbd "C-<return>") 'term-send-prompt
                (kbd "M-<left>") 'multi-term-prev
                (kbd "M-<right>") 'multi-term-next
                (kbd "M-<backspace>")   'term-delete-line-back
                (kbd "C-<backspace>")   'term-send-backward-kill-word
                (kbd "C-r")         '(lambda () (interactive) (term-send-raw-string "\C-r"))
                )
  ;; NOTE: Remember that moving the cursor in Emacs does not (always) move the
  ;; cursor in term
  (fill-keymap evil-insert-state-local-map
               (kbd "<return>") 'term-send-input)
  (fill-keymap evil-visual-state-local-map
               (kbd "<return>") 'term-send-region)
  )

(add-hook 'term-mode-hook 'jsrn-term-mode-hook)
(defun last-term-buffer (l)
  "Return most recently used term buffer given a list of buffers."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))
(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))





(message "Loaded terminal_setup.el")
(provide 'terminal_setup)
