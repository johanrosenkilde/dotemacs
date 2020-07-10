(require 'lsp-ui)
(require 'lsp-treemacs)
(require 'lsp-mode)

(require 'flycheck_setup)

;; (require 'lsp-clients)
(setq lsp-enable-snippet nil)

(setq lsp-headerline-breadcrumb-mode t)

(setq lsp-enable-imenu nil)

(setq lsp-ui-doc-position 'top)
(setq lsp-ui-doc-max-height 20)
(setq lsp-ui-doc-max-width 50)
(setq lsp-ui-doc-delay 1)

(fill-keymap lsp-mode-map
             (kbd "C-c C-t")  'lsp-describe-thing-at-point
             (kbd "C-c d")    'lsp-ui-doc-focus-frame
             (kbd "M-]")      'lsp-ui-find-next-reference
             (kbd "M-[")      'lsp-ui-find-prev-reference
             (kbd "C-c G")    'lsp-goto-type-definition
             )

(fill-keymap lsp-ui-doc-frame-mode-map
             (kbd "C-c d")    'lsp-ui-doc-unfocus-frame
             )



(message "Loaded lsp_setup.el")
(provide 'lsp_setup)
