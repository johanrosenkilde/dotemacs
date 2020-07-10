(require 'lsp_setup)

(setenv "PATH" (concat (getenv "PATH") ":/home/jsrn/.cargo/bin"))

(require 'rust-mode)
(require 'cargo)

(setq lsp-rust-server 'rust-analyzer)

(add-hook 'rust-mode-hook #'lsp)

(setq lsp-rust-show-warnings nil)

(fill-keymap rust-mode-map
             [(f2)] 'compile
             )

;; ;; Add keybindings for interacting with Cargo
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (rust-mode)

(message "Loaded rust_setup.el")
(provide 'rust_setup)
