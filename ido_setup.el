(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode)

(defun swiper-search-word ()
  "Search the current word in Swiper"
  (interactive)
  (swiper (current-word)))

(fill-keymap evil-normal-state-map
             (kbd "C-s")     'swiper
             (kbd "C-*")     'swiper-search-word
             (kbd "C-c C-r") 'ivy-resume
             (kbd "C-M-p")   'counsel-yank-pop
             )

(setq magit-completing-read-function 'ivy-completing-read)

(define-key ivy-mode-map [escape] 'minibuffer-keyboard-quit)

;; (require 'ido)
;; (require 'smex)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t) ;match substr on what is written
;; (setq ido-use-filename-at-point nil)
;; (setq ido-file-extensions-order '(".tex" ".sage" ".py" ".bib" ".txt"))
;; (setq ido-auto-merge-work-directories-length -1) ; don't suggest stuff in other dirs
;; (global-set-key "\M-x" 'smex) ;; awesome function chooser
;; (add-to-list 'ido-ignore-buffers "*terminal")
;; (ido-mode t)

;; ;; Use smex for C-h f
;; (defun  smex-describe-function (fun &optional commandp)
;;   "As `describe-function' but use smex completion."
;;   (interactive
;;    (list (let* ((fn (or (and (fboundp 'symbol-nearest-point)
;;                              (symbol-nearest-point))
;;                         (function-called-at-point)))
;;                 (smex-prompt-string "Describe function: "))
;;            (smex-completing-read (if fn (cons (symbol-name fn) list-of-all-functions) list-of-all-functions) nil))))
;;   (describe-function (intern fun))
;;   )
;; (global-set-key (kbd "C-h f") 'smex-describe-function)

;; Grabbed from EmacsWiki 03/02/2014
;; http://www.emacswiki.org/emacs/ImenuMode#toc8
;; 

;; (defun ido-goto-symbol (&optional symbol-list)
;;    "Refresh imenu and jump to a place in the buffer using Ido."
;;    (interactive)
;;    (unless (featurep 'imenu)
;;      (require 'imenu nil t))
;;    (cond
;;     ((not symbol-list)
;;      (let ((ido-mode ido-mode)
;;            (ido-enable-flex-matching
;;             (if (boundp 'ido-enable-flex-matching)
;;                 ido-enable-flex-matching t))
;;            name-and-pos symbol-names position)
;;        (unless ido-mode
;;          (ido-mode 1)
;;          (setq ido-enable-flex-matching t))
;;        (while (progn
;;                 (imenu--cleanup)
;;                 (setq imenu--index-alist nil)
;;                 (ido-goto-symbol (imenu--make-index-alist))
;;                 (setq selected-symbol
;;                       (ido-completing-read "Symbol? " symbol-names))
;;                 (string= (car imenu--rescan-item) selected-symbol)))
;;        (unless (and (boundp 'mark-active) mark-active)
;;          (push-mark nil t nil))
;;        (setq position (cdr (assoc selected-symbol name-and-pos)))
;;        (cond
;;         ((overlayp position)
;;          (goto-char (overlay-start position)))
;;         (t
;;          (goto-char position)))))
;;     ((listp symbol-list)
;;      (dolist (symbol symbol-list)
;;        (let (name position)
;;          (cond
;;           ((and (listp symbol) (imenu--subalist-p symbol))
;;            (ido-goto-symbol symbol))
;;           ((listp symbol)
;;            (setq name (car symbol))
;;            (setq position (cdr symbol)))
;;           ((stringp symbol)
;;            (setq name symbol)
;;            (setq position
;;                  (get-text-property 1 'org-imenu-marker symbol))))
;;          (unless (or (null position) (null name)
;;                      (string= (car imenu--rescan-item) name))
;;            (add-to-list 'symbol-names name)
;;            (add-to-list 'name-and-pos (cons name position))))))))


;;; All done
;;;
(message "Loaded ido_setup.el")
(provide 'ido_setup)



;; Default Ido Key Map
;;
;; Basic map
;; | C-a     | 'ido-toggle-ignore              |
;; | C-c     | 'ido-toggle-case                |
;; | C-e     | 'ido-edit-input                 |
;; | Tab     | 'ido-complete                   |
;; | Space   | 'ido-complete-space             |
;; | C-j     | 'ido-select-text                |
;; | C-m     | 'ido-exit-minibuffer            |
;; | C-p     | 'ido-toggle-prefix (OVERRIDDEN) |
;; | C-r     | 'ido-prev-match                 |
;; | C-s     | 'ido-next-match                 |
;; | C-t     | 'ido-toggle-regexp              |
;; | C-z     | 'ido-undo-merge-work-directory  |
;; | C-Space | 'ido-restrict-to-matches        |
;; | M-Space | 'ido-take-first-match           |
;; | C-@     | 'ido-restrict-to-matches        |
;; | Right   | 'ido-next-match                 |
;; | Left    | 'ido-prev-match                 |
;; | ?       | 'ido-completion-help            |
;;
;; Magic commands.
;; | C-b | 'ido-magic-backward-char |
;; | C-f | 'ido-magic-forward-char  |
;; | C-d | 'ido-magic-delete-char   |
;;
;; File and directory map
;; | C-x C-b                      | 'ido-enter-switch-buffer                 |
;; | C-x C-f                      | 'ido-fallback-command                    |
;; | C-x C-d                      | 'ido-enter-dired                         |
;; | Down                         | 'ido-next-match-dir                      |
;; | Up                           | 'ido-prev-match-dir                      |
;; | M-Up                         | 'ido-prev-work-directory                 |
;; | M-Down                       | 'ido-next-work-directory                 |
;; | Backspace                    | 'ido-delete-backward-updir               |
;; | Delete                       | 'ido-delete-backward-updir               |
;; | [remap delete-backward-char] | 'ido-delete-backward-updir) ; B          |
;; | [remap backward-kill-word]   | 'ido-delete-backward-word-updir)  ; M-DE |
;; | C-Backspace                  | 'ido-up-directory                        |
;; | C-l                          | 'ido-reread-directory                    |
;; | M-d                          | 'ido-wide-find-dir-or-delete-dir         |
;; | M-b                          | 'ido-push-dir                            |
;; | M-v                          | 'ido-push-dir-first                      |
;; | M-f                          | 'ido-wide-find-file-or-pop-dir           |
;; | M-k                          | 'ido-forget-work-directory               |
;; | M-m                          | 'ido-make-directory                      |
;; | M-n                          | 'ido-next-work-directory                 |
;; | M-o                          | 'ido-prev-work-file                      |
;; | M-C-o                        | 'ido-next-work-file                      |
;; | M-p                          | 'ido-prev-work-directory                 |
;; | M-s                          | 'ido-merge-work-directories              |
;;
;; File only map
;; | C-k | 'ido-delete-file-at-head                                         |
;; | C-o | 'ido-copy-current-word                                           |
;; | C-w | 'ido-copy-current-file-name (Insert file name of current buffer) |
;; | M-l | 'ido-toggle-literal                                              |
;;
;; Buffer map
;; | C-x C-f | 'ido-enter-find-file        |
;; | C-x C-b | 'ido-fallback-command       |
;; | C-k     | 'ido-kill-buffer-at-head    |
;; | C-o     | 'ido-toggle-virtual-buffers |


