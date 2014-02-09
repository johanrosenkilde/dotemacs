;; Copyright (C) 2013 Johan S. R. Nielsen

;; Author: Johan S. R. Nielsen <jsrn@jsrn.dk>
;; Keywords: desktop

;;; Commentary:

;;; This file adds a small amount of extra functionality on top of desktop.el to
;;; support easy, dynamic switching betweek different desktops.

(defcustom mdesktop-base-file-name "desktop"
  "The base file name to use for desktop files"
  :type 'string
  :group 'mdesktop)

(defcustom mdesktop-base-dir "~/.emacs.d/desktops/"
  "The base directory where desktops are saved"
  :type 'string
  :group 'mdesktop)

(setq mdesktop-current nil)

(defun mdesktop-save-current ()
  "Saves the current desktop"
  (interactive)
  (if mdesktop-current
      (desktop-save desktop-dirname t)
    (error "No current desktop set")
    ))

(defun mdesktop-save-new (desktop)
  "Save the current desktop as a new desktop"
  (interactive "sName of desktop: ")
  (setq mdesktop-current desktop)
  (setq desktop-dirname (concat desktop-base-dir mdesktop-current))
  (mkdir desktop-dirname t)
  (mdesktop-save-current))

(defun mdesktop-discard ()
  "Discard the current desktop without saving and clear everything"
  (interactive)
  (if (y-or-n-p "Are you sure you wish to discard the current desktop without saving?")
    (progn
      (setq mdesktop-current nil)
      (desktop-clear))))

(defun mdesktop-put-away-current-for-switch ()
  "Save the current desktop and clears as preparation for a desktop switch.
   Usually not necessary to call directly"
  (interactive)
  (if (eq mdesktop-current nil)
      (if (y-or-n-p "Do you wish to save your current unnamed desktop first?")
          (call-interactively 'mdesktop-save-new))
    (mdesktop-save-current))
  (desktop-clear)
  (setq mdesktop-current nil))

(defun mdesktop-create-new (desktop)
  "Create a new, blank desktop. Saves the current desktop first"
  (interactive "sName of desktop: ")
  (mdesktop-put-away-current-for-switch)
  (mdesktop-save-new desktop))

(defun mdesktop-switch (desktop)
  (interactive (list (completing-read "Switch to desktop: "
                                      (directory-files mdesktop-base-dir))))
  (if (file-exists-p (concat mdesktop-base-dir desktop))
      (progn
        (mdesktop-put-away-current-for-switch)
        (setq mdesktop-current desktop)
        (setq desktop-dirname (concat mdesktop-base-dir mdesktop-current))
        (desktop-read desktop-dirname))
    (error "The desktop %s does not exist" desktop)))

(defun mdesktop-save-on-kill-emacs ()
  "Save the current desktop, if set, when emacs dies. Never query the user."
  (interactive)
  (if mdesktop-current
      (desktop-save desktop-dirname t)))
(add-hook 'kill-emacs-hook 'mdesktop-save-on-kill-emacs)

(provide 'mdesktop)
