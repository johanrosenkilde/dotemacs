(require 'tuareg)

;; Activate OPAM, the Ocaml package manager
(setq opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share"))))
(add-to-list `load-path  (expand-file-name "emacs/site-lisp" opam-share))

;; Activate Merlin for command completion and type lookup etc.
(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'caml-mode-hook 'merlin-mode)
(setq merlin-command 'opam)
(setq merlin-ac-setup 'easy)

(defun jsrn-tuareg-mode-hook ()
  (setq tab-width 2)
  (setq tuareg-indent-align-with-first-arg nil)
  (setq tuareg-match-patterns-aligned t)
  (electric-indent-mode 0)
  (merlin-mode)
  ;; (setq compilation-environment
  ;;       (with-temp-buffer
  ;;         (ignore-errors (call-process "opam" nil t nil "config" "-env"))
  ;;         (goto-line 1)
  ;;         (while (re-search-forward "\"\\(.*\\)\"; *export.*$" nil t)
  ;;           (replace-match "\\1" nil nil))
  ;;         (split-string (buffer-substring 1 (point-max)))
  ;;         ))
  (defun ocaml-send-current-block ()
    "Find last blank line and next blank line, and send all in between
to OCaml buffer"
    (interactive)
    (save-excursion
      (evil-backward-paragraph)
      (let ((beg (point)))
        (evil-forward-paragraph)
        (tuareg-eval-region beg (point))
      ))
    )
  (defun ocaml-goto-shell()
    "Find the OCaml shell and show it"
    (interactive)
    (switch-to-buffer
     (-first  (lambda (buf) (string-match "ocaml-toplevel" (buffer-name buf)))
                  (buffer-list))))
  (fill-keymap tuareg-mode-map
               (kbd "C-<return>") 'ocaml-send-current-block
               (kbd "M-RET")   'tuareg-eval-region
               (kbd "C-c C-c") 'tuareg-eval-buffer
               (kbd "C-SPC")   'completion-at-point
               (kbd "C-c C-z") 'ocaml-goto-shell)
  (fill-keymap merlin-mode-map
               [(f2)]              'merlin-type-enclosing
               (kbd "C-<up>")    'merlin-type-enclosing-go-up
               (kbd "C-<down>")  'merlin-type-enclosing-go-down
               (kbd "C-<right>") 'merlin-type-enclosing
               [(f3)]             'merlin-locate
               [(shift f3)]       'merlin-pop-stack
               [(f7)]         'merlin-error-next
               [(shift f7)]   'merlin-error-prev
               [(f8)]          'merlin-switch-to-ml
               [(shift f8)]    'merlin-switch-to-mli)
  )
(add-hook 'tuareg-mode-hook 'jsrn-tuareg-mode-hook)



(message "Loaded ocaml_setup.el")
(provide 'ocaml_setup)
