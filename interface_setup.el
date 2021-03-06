;; SETUP FOR THE EMACS INTERFACE

(setq workman t) ;; Keyboard layout to expect
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(scroll-bar-mode -1) ;; Emacs gurus don't need no stinking scroll bars
(menu-bar-mode 0)    ;; or menu bars
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono-8"))
;; (add-to-list 'default-frame-alist '(left-fringe . 0))
;; (add-to-list 'default-frame-alist '(right-fringe . 0))

; Fix idiotic visual-bell on OS X
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(setq default-font-height 80)

;; Themes
(setq zenburn-use-variable-pitch t)
(setq zenburn-scale-org-headlines t)
(setq dark-theme 'zenburn)
(setq light-theme 'solarized-light)
(defun set-theme-light ()
  (interactive)
  (disable-theme dark-theme)
  (load-theme light-theme t)
)
(defun set-theme-dark ()
  (interactive)
  (disable-theme light-theme)
  (load-theme dark-theme t)
)
(set-theme-dark)

;; (set-face-attribute 'default nil :height default-font-height)
