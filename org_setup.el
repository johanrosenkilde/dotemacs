;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Emacs-wide loads, vars etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-deadline-warning-days 7
      )

(fill-keymaps (list org-mode-map)
              (kbd (concat "M-" evil-left-key))  'org-metaleft
              (kbd (concat "M-" evil-down-key))  'org-metadown
              (kbd (concat "M-" evil-up-key))    'org-metaup
              (kbd (concat "M-" evil-right-key)) 'org-metaright)

(fill-keymap org-mode-map
             (kbd (concat "M-" evil-left-key-uc))  'org-shiftmetaleft
             (kbd (concat "M-" evil-down-key-uc))  'org-shiftmetadown
             (kbd (concat "M-" evil-up-key-uc))    'org-shiftmetaup
             (kbd (concat "M-" evil-right-key-uc)) 'org-shiftmetaright
             (kbd (concat "C-M-" evil-up-key))    'org-backward-element
             (kbd (concat "C-M-" evil-down-key)) 'org-forward-element
             (kbd "C-c l") 'org-store-link ;; insert it with C-c C-l (org-insert-link)
             )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for setting up each buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jsrn-org-mode-hook ()
  (visual-line-mode t)
  (org-indent-mode)
  (evil-declare-motion 'org-up-element)
  ;; to override evil binding for ~, we do it on the evil local maps
  (fill-keymaps (list evil-motion-state-local-map
                      evil-visual-state-local-map
                      evil-normal-state-local-map)
                (kbd "~")  (lambda () (interactive) (progn
                                                      (evil-set-jump);TODO: Necessary?
                                                      (org-up-element))))
  ;; Let org mode override M-n
  (define-key evil-normal-state-local-map (kbd "M-n") 'org-metadown)
  
)
(add-hook 'org-mode-hook 'jsrn-org-mode-hook)
;; This file is excecuted when an org buffer is opened for the first time, so
;; the above hook is not run for that file. Therefore, run the hook.
(jsrn-org-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatted Calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'calfw)
(require 'calfw-org)

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)



(message "Loaded org_setup.el")
(provide 'org_setup)
