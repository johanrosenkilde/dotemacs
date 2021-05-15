;;; Speed up init
;;; Copied from Ambrevar: https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/init.el
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'reset-file-name-handler-alist)

;; Show startup-time after load
(add-hook 'after-init-hook (lambda ()
  (message (format "Emacs initialized: took %f seconds." (float-time (time-subtract after-init-time before-init-time))))))
