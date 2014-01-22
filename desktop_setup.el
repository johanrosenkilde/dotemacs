(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-base-file-name "desktop")
(setq jsrn-desktop-current nil)
(defun desktop-save-current ()
  "Saves the current desktop"
  (interactive)
  (if jsrn-desktop-current
      (desktop-save desktop-dirname t)
    (error "No current desktop set")
    ))

(defun desktop-save-new (desktop)
  "Save the current desktop as a new desktop"
  (interactive "sName of desktop: ")
  (setq jsrn-desktop-current desktop)
  (setq desktop-dirname (concat jsrn-desktop-base-dir jsrn-desktop-current))
  (mkdir dirname t)
  (desktop-save-current))

(defun desktop-discard ()
  "Discard the current desktop without saving and clear everything"
  (interactive)
  (if (y-or-n-p "Are you sure you wish to discard the current desktop without saving?")
    (progn
      (setq jsrn-desktop-current nil)
      (desktop-clear))))

(defun desktop-put-away-current-for-switch ()
  "Save the current desktop and clears as preparation for a desktop switch.
   Usually not necessary to call directly"
  (interactive)
  (if (eq jsrn-desktop-current nil)
      (if (y-or-n-p "Do you wish to save your current unnamed desktop first?")
          (call-interactively 'desktop-save-new))
    (desktop-save-current))
  (desktop-clear)
  (setq jsrn-desktop-current nil))

(defun desktop-create-new (desktop)
  "Create a new, blank desktop. Saves the current desktop first"
  (interactive "sName of desktop: ")
  (desktop-put-away-current-for-switch)
  (desktop-save-new desktop))

(defun desktop-switch (desktop)
  (interactive (list (completing-read "Switch to desktop: "
                                      (directory-files jsrn-desktop-base-dir))))
  (if (file-exists-p (concat jsrn-desktop-base-dir desktop))
      (progn
        (desktop-put-away-current-for-switch)
        (setq jsrn-desktop-current desktop)
        (setq desktop-dirname (concat jsrn-desktop-base-dir jsrn-desktop-current))
        (desktop-read desktop-dirname))
    (error "The desktop %s does not exist" desktop)))

(defun desktop-save-on-kill-emacs ()
  "Save the current desktop, if set, when emacs dies. Never query the user"
  (interactive)
  (if jsrn-desktop-current
      (desktop-save desktop-dirname t)))
(add-hook 'kill-emacs-hook 'desktop-save-on-kill-emacs)
