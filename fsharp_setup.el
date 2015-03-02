(require 'fsharp-mode)

(defun jsrn-fsharp-mode-hook ()
  (setq evil-shift-width 2)
  (column-number-mode)
  (defun fsharp-send-current-block ()
    "Find last blank line and next blank line, and send all in between
to Fsharp buffer"
    (interactive)
    (save-excursion
      (evil-backward-paragraph)
      (let ((beg (point)))
        (evil-forward-paragraph)
        (fsharp-eval-region beg (point))
      ))
    )
  (defun jsrn-fsharp-load-files (files)
    "Reload each file of the list of files into the inferior buffer"
    (interactive)
    (save-excursion
      (fsharp-run-process-if-needed)
      (dolist (file files)
        (fsharp-simple-send inferior-fsharp-buffer-name (concat "#load \"" file "\"")))
      ))
  (defun jsrn-fsharp-reload-project-entire ()
    "Reload ALL files of the project into the inferior buffer, including the
last main file"
    (interactive)
    (save-some-buffers)
    (jsrn-fsharp-load-files fsharp-ac-project-files)
    (fsharp-show-subshell)
    )
  (defun jsrn-fsharp-reload-project-libs ()
    "Reload all but the last file of the project into the inferior buffer"
    (interactive)
    (save-some-buffers)
    (jsrn-fsharp-load-files (butlast fsharp-ac-project-files))
    (fsharp-show-subshell)
  )
  (fill-keymap fsharp-mode-map
               (kbd "C-<return>") 'fsharp-send-current-block
               (kbd "M-RET")   'fsharp-eval-region
               (kbd "C-SPC")   'completion-at-point
               (kbd "C-c k")   'fsharp-goto-block-up
               [(f5)]          'jsrn-fsharp-reload-project-libs
               [(shift f5)]    'jsrn-fsharp-reload-project-entire
               (kbd "C-c C-z") '(lambda () (interactive)
                                  (fsharp-show-subshell) (other-window 1)))
)
(add-hook 'fsharp-mode-hook 'jsrn-fsharp-mode-hook)

(defun jsrn-inferior-fsharp-mode-hook ()
  (interactive)
  (fill-keymap evil-insert-state-local-map
               (kbd "<return>") 'fsharp-comint-send)
  (fill-keymap inferior-fsharp-mode-map
               (kbd "C-d")     '(lambda () (interactive) (evil-scroll-down 20))
               (kbd "RET")     'fsharp-comint-send
               ))
(add-hook 'inferior-fsharp-mode-hooks 'jsrn-inferior-fsharp-mode-hook) ;; note: non-standard hook

(defun fsharpi-fix-ac ()
  "Auto-complete regularly crashes. When it does, run this function to
fix it again."
  (interactive)
  (setq ac-cursor-color "red")
  (auto-complete-mode 1)
  )





(message "Loaded fsharp_setup.el")
(provide 'fsharp_setup)
