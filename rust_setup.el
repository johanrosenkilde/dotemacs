(require 'lsp_setup)

(setenv "PATH" (concat (getenv "PATH") ":/home/jsrn/.cargo/bin"))

(require 'rust-mode)
(require 'cargo)

(setq lsp-rust-server 'rls)
;; (setq lsp-rust-server 'rust-server)

(add-hook 'rust-mode-hook #'lsp)

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'rust-mode-hook #'eldoc-mode)
;; (add-hook 'rust-mode-hook #'company-mode)
;; (setq company-tooltip-align-annotations t)

(setq lsp-rust-show-warnings nil)

(fill-keymap rust-mode-map
             [(f2)] 'compile
             ;; (kbd "TAB") 'company-indent-or-complete-common
             )

;; ;; Add keybindings for interacting with Cargo
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook #'undo-tree-mode)

;; (rust-mode)

(message "Loaded rust_setup.el")
(provide 'rust_setup)
