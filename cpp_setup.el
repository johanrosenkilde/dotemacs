(require 'lsp_setup)

(require 'lsp-clangd)
(setq lsp-c++-server 'clangd)

;; (require 'ccls)
;; (setq ccls-executable "ccls")
;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))

(setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      ;; treemacs-space-between-root-nodes nil
      ;; company-idle-delay 0.0
      ;; company-minimum-prefix-length 1
      lsp-idle-delay 0.1 ;; clangd is fast
      ;; be more ide-ish
      lsp-headerline-breadcrumb-enable t
      lsp-enable-indentation nil ;; Bug in lsp + evil: https://github.com/syl20bnr/spacemacs/issues/13541)
      )

(defun jsrn-c++-mode-hook ()
  (lsp)
  ;; (ccls-mode)
  (undo-tree-mode) ; For some reason sometimes gets deactivated
  (projectile-mode)
  )
(add-hook 'c++-mode-hook 'jsrn-c++-mode-hook)
(add-hook 'cpp-mode-hook 'jsrn-c++-mode-hook)
(add-hook 'cc-mode-hook 'jsrn-c++-mode-hook)

;; This file is excecuted when a buffer is opened, so the above hook is not run
;; for that file. Therefore, run the hook.
(jsrn-c++-mode-hook)

;; ;; GDB for C/C++
;; (setq gdb-many-windows t)
;; (setq gdb-speedbar-auto-raise t)
;; (defun jsrn-gdb-mode-hook ()
;;   (interactive)
;;   (set-fringe-style 'default)
;;   (define-key evil-normal-state-local-map (kbd "C-p") 'gud-print)
;;   )
;; (add-hook 'gdb-frames-mode-hook 'jsrn-gdb-mode-hook)

(message "Loaded cpp_setup")
(provide 'cpp_setup)
