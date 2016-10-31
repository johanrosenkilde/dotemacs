;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Emacs-wide loads, vars etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sage-shell-mode-path "/home/jsrn/local/sage-shell-mode")
(add-to-list 'load-path sage-shell-mode-path)
(require 'sage-shell-mode)
(require 'sage-shell-blocks)

;; (setq sage-shell:sage-root  "/home/jsrn/local/sage/sage_stable")
;; (setq sage-shell:use-prompt-toolkit nil)
(setq sage-shell:sage-root  "/home/jsrn/local/sage/sage_devel")
(setq sage-shell:use-prompt-toolkit t)

;; Turn on eldoc-mode
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

(setq sage-shell:input-history-cache-file "~/.emacs.d/.sage_shell_input_history")

;; Define short names for modes
(sage-shell:define-alias)

(define-key sage-shell-mode-map (kbd "C-SPC") 'jsrn-scroll-up)
(define-key sage-shell-mode-map (kbd "M-C-SPC") 'jsrn-scroll-down)

;; (define-key python-mode-map (kbd "C-<backspace>") 'backward-kill-word)

;; TODO: Fix this for sage-shell-mode. Add make pull request to add it
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


(fill-keymap sage-shell:sage-mode-map
             (kbd "C-c C") 'sage-send-class
             )

(evil-declare-motion 'sage-shell-blocks:forward)
(evil-declare-motion 'sage-shell-blocks:backward)
(defadvice sage-shell-blocks:send-current (before send-shell-blocks:send-current-save-point activate)
  "Before sending the current block, save point in jump list"
  (evil-set-jump))

(fill-keymap sage-shell:sage-mode-map
             (kbd "C-<return>") 'sage-shell-blocks:send-current
             (kbd "M-{")        'sage-shell-blocks:backward
             (kbd "M-}")        'sage-shell-blocks:forward
             (kbd "C-<backspace>")     'backward-kill-word
             (kbd "C-c C-j")    'sage-shell:send-doctest
               )


(fill-keymap sage-shell-mode-map
              (kbd "C-<return>") 'sage-shell-blocks:pull-next
              (kbd "<f5>")       '(lambda () (interactive) (hs-hide-all) (recenter-top-bottom)))






;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FROM sage-mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq sage-mode-path "/home/jsrn/local/sage-mode/sage-mode-jsrn/emacs")
;; (add-to-list 'load-path sage-mode-path)
;; (setq sage-path "/home/jsrn/local/sage/sage_stable")
;; (require 'sage "sage")
;; (require 'sage-view "sage-view")
;; (require 'sage-blocks "sage-blocks")

;; (require 'hideshow)
;; (setq sage-view-anti-aliasing-level 4
;;       sage-view-scale 1.0
;;       sage-view-default-commands t
;;       sage-view-scale-factor 1)
;; (setq sage-command (cl-concatenate 'string sage-path "/sage"))
;; (evil-set-initial-state 'inferior-sage-mode 'normal)
;; (setq python-guess-indent nil)
;; (setq python-indent-offset 4)

;; (add-hook 'sage-startup-after-prompt-hook 'sage-view)

;; ;; Since Sage is constatly restarted, add functionality to find it again
;; (defun sage-refind-sage ()
;;   "Ensure that the local buffer's sage points to a running process. Otherwise,
;;   find a running sage process for it, or return nil"
;;   (interactive)
;;   (if (and sage-buffer (buffer-name sage-buffer))
;;       sage-buffer
;;     (progn
;;       (let ((buf (-first  (lambda (buf) (string-match "Sage-main" (buffer-name buf)))
;;                           (buffer-list))))
;;         (when buf
;;           (setq sage-buffer buf))
;;         buf))
;;   ))

;; ;; Advices to find it whatever is being done
;; (defadvice sage-send-buffer (before sage-send-region-refind-sage activate)
;;   (sage-refind-sage))
;; (defadvice sage-send-region (before sage-send-region-refind-sage activate)
;;   (sage-refind-sage))

;; (defun sage-restart ()
;;   (interactive)
;;   (let ((old-sage (sage-refind-sage)))
;;     (when (and old-sage (buffer-name old-sage)) ; test if sage-buffer is defined and not killed
;;       ;; get the sage process and unset its query flag
;;       (set-process-query-on-exit-flag (get-buffer-process sage-buffer) nil)
;;       (kill-buffer sage-buffer)
;;       (setq sage-buffer nil)))
;;   (let ((cmd (if sage-run-history (car sage-run-history) sage-command)))
;;     (sage t cmd)))

;; (fill-keymap sage-mode-map
;;              (kbd "C-c C") 'sage-send-class
;;              (kbd "C-c C-z") 'run-sage
;;              )

;; (defun sage-fix-preview ()
;;   "This is a workaround for a bug in sage-mode preview, where opening a tex file
;;   in the Emacs process will break further sage-view functionality.
;;   AUCTeX sets a variable TEXINPUTS to make latex look for its version of
;;   preview.sty, but it seems that this version does not work with sage-mode."
;;   (interactive)
;;   (setenv "TEXINPUTS" "")
;;   )
;; (add-hook 'inferior-sage-mode-hook 'jsrn-inferior-sage-mode-hook)

;; (evil-declare-motion 'sage-forward-block)
;; (evil-declare-motion 'sage-backward-block)
;; (defadvice sage-send-current-block (before send-current-block-save-point activate)
;;   "Before sending the current block, save point in jump list"
;;   (evil-set-jump))
;; (fill-keymap sage-mode-map
;;              (kbd "C-<return>") 'sage-send-current-block
;;              (kbd "M-{")        'sage-backward-block
;;              (kbd "M-}")        'sage-forward-block)

;; (fill-keymap inferior-sage-mode-map
;;               (kbd "C-<return>") 'sage-pull-next-block
;;               (kbd "<f5>")       '(lambda () (interactive) (hs-hide-all) (recenter-top-bottom)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Function for setting up each buffer
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun jsrn-sage-mode-hook ()
;;   (interactive)
;;   ;; Nothing yet
;;   )
;; (add-hook 'sage-mode-hook 'jsrn-sage-mode-hook)

;; ;; This file is excecuted when a python buffer is opened, so the above hook is
;; ;; not run for that file. Therefore, run the hook.
;; (jsrn-sage-mode-hook)


;; (defun jsrn-inferior-sage-mode-hook ()
;;   (interactive)
;;   (fill-keymap evil-insert-state-local-map
;;                (kbd "<return>") 'comint-send-input)
;;   )






;; (setq ansi-color-drop-regexp
;;       " \\[\\([ABCDsuK]\\|[12][JK]\\|=[0-9]+[hI]\\|[0-9;]*[HfDnC]\\|\\?[0-9]+[hl]\\|J\\)")

;; (defun ansi-color-filter-apply (string)
;;   "Filter out all ANSI control sequences from STRING.

;; Every call to this function will set and use the buffer-local variable
;; `ansi-color-context' to save partial escape sequences.  This information
;; will be used for the next call to `ansi-color-apply'.  Set
;; `ansi-color-context' to nil if you don't want this.

;; This function can be added to `comint-preoutput-filter-functions'."
;;   (let ((start 0) end result)
;;     ;; if context was saved and is a string, prepend it
;;     (if (cadr ansi-color-context)
;;         (setq string (concat (cadr ansi-color-context) string)
;;               ansi-color-context nil))
;;     ;; eliminate unrecognized escape sequences
;;     (while (string-match ansi-color-drop-regexp string)
;;       (setq string
;;             (replace-match "" nil nil string)))
;;     ;; find the next escape sequence
;;     (while (setq end (string-match ansi-color-regexp string start))
;;       (setq result (concat result (substring string start end))
;;             start (match-end 0)))
;;     ;; save context, add the remainder of the string to the result
;;     (let (fragment)
;;       (if (string-match "\033" string start)
;; 	  (let ((pos (match-beginning 0)))
;; 	    (setq fragment (substring string pos)
;; 		  result (concat result (substring string start pos))))
;; 	(setq result (concat result (substring string start))))
;;       (setq ansi-color-context (if fragment (list nil fragment))))
;;     result))








(message "Loaded sage_setup.el")
(provide 'sage_setup)
