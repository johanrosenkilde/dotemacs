;; Specify a folder where all emacs-related stuff should be
;; Most files will be in the .emacs.d sub-folder of emacs-home
(setenv "HOME" emacs-home)
(setq user-emacs-directory (concat emacs-home "/"))
(setq init-home (concat emacs-home "/setup"))
(add-to-list 'load-path init-home)
;; Setup you do through the menu system will end in the following file
(setq custom-file (concat init-home "/emacs-custom.el"))

;; From the Linux HOME/.emacs
(package-initialize)
(load "init")
(load custom-file)
