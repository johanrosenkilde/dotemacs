;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Emacs-wide loads, vars etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sage-mode-path "/home/jsrn/local/sage-mode/sage-mode-jsrn/emacs")
(add-to-list 'load-path sage-mode-path)
(setq sage-path "/home/jsrn/local/sage/sage-6.3")
(require 'sage "sage")
(require 'sage-view "sage-view")
(require 'sage-blocks "sage-blocks")

(require 'hideshow)
(setq sage-view-anti-aliasing-level 4
      sage-view-scale 1.0
      sage-view-default-commands t
      sage-view-scale-factor 1)
(setq sage-command (cl-concatenate 'string sage-path "/sage"))
(evil-set-initial-state 'inferior-sage-mode 'normal)
(setq python-guess-indent nil)
(setq python-indent-offset 4)

(add-hook 'sage-startup-after-prompt-hook 'sage-view)

(define-key inferior-sage-mode-map (kbd "C-SPC") 'jsrn-scroll-up)
(define-key inferior-sage-mode-map (kbd "M-C-SPC") 'jsrn-scroll-down)
(define-key sage-mode-map (kbd "C-c C-h") 'sage-pcomplete-or-help)

(define-key python-mode-map (kbd "C-<backspace>") 'backward-kill-word)

;; Since Sage is constatly restarted, add functionality to find it again
(defun sage-refind-sage ()
  "Ensure that the local buffer's sage points to a running process. Otherwise,
  find a running sage process for it, or return nil"
  (interactive)
  (if (and sage-buffer (buffer-name sage-buffer))
      sage-buffer
    (progn
      (let ((buf (-first  (lambda (buf) (string-match "Sage-main" (buffer-name buf)))
                          (buffer-list))))
        (when buf
          (setq sage-buffer buf))
        buf))
  ))

;; Advices to find it whatever is being done
(defadvice sage-send-buffer (before sage-send-region-refind-sage activate)
  (sage-refind-sage))
(defadvice sage-send-region (before sage-send-region-refind-sage activate)
  (sage-refind-sage))

(defun sage-send-class ()
  (interactive)
  ;;TODO: sage-refind-sage?
  (save-excursion
    (search-backward-regexp "^class ") ; find a class line or error
    (let ((begin (point)))
      (push-mark) ; for history jumping
      (next-line)
      (when (search-forward-regexp "^[^ \\t\n]" nil 1) ; find first non-indented line
        (backward-char)) ; go to right before if we are not at file end
      (sage-send-region begin (point))
      )))

(defun sage-restart ()
  (interactive)
  (let ((old-sage (sage-refind-sage)))
    (when (and old-sage (buffer-name old-sage)) ; test if sage-buffer is defined and not killed
      ;; get the sage process and unset its query flag
      (set-process-query-on-exit-flag (get-buffer-process sage-buffer) nil)
      (kill-buffer sage-buffer)
      (setq sage-buffer nil)))
  (let ((cmd (if sage-run-history (car sage-run-history) sage-command)))
    (sage t cmd)))
(fill-keymap sage-mode-map
             (kbd "C-c C") 'sage-send-class
             (kbd "C-c C-z") 'run-sage
             )

(defun sage-fix-preview ()
  "This is a workaround for a bug in sage-mode preview, where opening a tex file
  in the Emacs process will break further sage-view functionality.
  AUCTeX sets a variable TEXINPUTS to make latex look for its version of
  preview.sty, but it seems that this version does not work with sage-mode."
  (interactive)
  (setenv "TEXINPUTS" "")
  )
(add-hook 'inferior-sage-mode-hook 'jsrn-inferior-sage-mode-hook)

(evil-declare-motion 'sage-forward-block)
(evil-declare-motion 'sage-backward-block)
(defadvice sage-send-current-block (before send-current-block-save-point activate)
  "Before sending the current block, save point in jump list"
  (evil-set-jump))
(fill-keymap sage-mode-map
             (kbd "C-<return>") 'sage-send-current-block
             (kbd "M-{")        'sage-backward-block
             (kbd "M-}")        'sage-forward-block)

(fill-keymap inferior-sage-mode-map
              (kbd "C-<return>") 'sage-pull-next-block
              (kbd "<f5>")       '(lambda () (interactive) (hs-hide-all) (recenter-top-bottom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for setting up each buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jsrn-sage-mode-hook ()
  (interactive)
  ;; Nothing yet
  )
(add-hook 'sage-mode-hook 'jsrn-sage-mode-hook)

;; This file is excecuted when a python buffer is opened, so the above hook is
;; not run for that file. Therefore, run the hook.
(jsrn-sage-mode-hook)


(defun jsrn-inferior-sage-mode-hook ()
  (interactive)
  (fill-keymap evil-insert-state-local-map
               (kbd "<return>") 'comint-send-input)
  )





(message "Loaded sage_setup.el")
(provide 'sage_setup)
