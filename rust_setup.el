(setenv "PATH" (concat (getenv "PATH") ":/home/jsrn/.cargo/bin"))
(require 'lsp-mode)
;; (require 'lsp-clients)
(require 'rust-mode)
(require 'cargo)

(setq lsp-enable-snippet nil)

(use-package rust-mode
  :hook (rust-mode . lsp))

;; ;; Add keybindings for interacting with Cargo
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (rust-mode)

(message "Loaded rust_setup.el")
(provide 'rust_setup)
