;; Copyright (C) 2014 Johan S. R. Nielsen

;; Author: Johan S. R. Nielsen <jsrn@jsrn.dk>
;; Keywords: password management

;;; Commentary:

;;; This file allows a simple never-look-at-it password management on
;;; top of EasyPG, which Emacs >=23 is born with.
;;;
;;; What you want is password management where
;;; 1) you never need to look at the whole file
;;; 2) auto-generating and adding passwords is trivial
;;; 3) looking up passwords is trivial and Ido-powered
;;; 4) you never need to look at any password: they go to the
;;; clipboard when you need it, and you paste them to where you need it.
;;; 5) The passwords are not stored in memory but are decrypted and
;;; read from the file when needed.
;;;
;;; This module does just that (and nothing more)
;;; Key<->Passwords mappings are stored in the file `secret-password-file',
;;; one line per each.
;;; You should end the name of this file with ".gpg" so EasyPG
;;; automatically encrypts/decrypts it.
;;; 
;;; To use, set `secret-password-file' and put the following in your
;;; .emacs:
;;;   (require 'secrets)
;;;   (secret-load-keys)
;;;
;;; Now you have the following functions: `secret-lookup',
;;; `secret-lookup-clipboard', `secret-new', `secret-update-password'
;;; for interacting with your secrets.
;;;
;;; Auto-generating passwords is done using a shell command which can
;;; be specified with `secret-generate-password-command'. By default
;;; it uses pwgen.

(defgroup simple-secrets nil
  "Group for the `simple-secrets' package"
  )
(defcustom secret-password-file "~/.secrets.gpg"
  "The file for storing passwords. It should end in .gpg for enabling encryption."
  :type 'string
  :group 'simple-secrets)

(defcustom secret-generate-password-command "pwgen -N 1 -ny"
  "The command for generating new random passwords"
  :type 'string
  :group 'simple-secrets)

;; Local variable containing all keys stored in the password list.
;; Populated by secret-load-keys
(setq secret-password-keys nil)

(defun secret-load-keys ()
  "Loads all keys in the password file. Note: This does not remember
the passwords in any Emacs variables; only the keys."
  (setq secret-password-keys nil)
  (with-temp-buffer
    (insert-file-contents secret-password-file)
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (let ((start (point)))
        (while (not (eq (char-after) ?\t))
          (forward-char))
        (add-to-list 'secret-password-keys
                     (buffer-substring start (point)))
        (forward-line)))
  ))

(defun secret-lookup (key)
  "Return the password for the given key."
  (interactive
   (list (ido-completing-read "Key or site: " secret-password-keys)))
  (with-temp-buffer
    (insert-file-contents secret-password-file)
    (condition-case nil
        (progn
          (re-search-forward (concat "^" key "\t\\(.*\\)$"))
          (match-string 1))
      (error (error "The key was not found.")))
  ))

(defun secret-lookup-clipboard (key)
  "Put the password for the given key into the clipboard and kill ring"
  (interactive
   (list (ido-completing-read "Key or site: " secret-password-keys)))
  (kill-new (secret-lookup key)))

(defun secret-new (key pass)
  "Add the key/password pair to the secret file. If password is nil or
empty string then autogenerate a password, and put it in the kill ring
after adding it to the secret file."
  (interactive "sName of new key: \nsPassword: ")
  (when (member key secret-password-keys)
    (error "A password is already stored for this key"))
  (when (string-match "\t" key)
    (error "Key is not allowed to contain tabs"))
  (when (or (not pass) (equal "" pass))
    (progn
      (setq pass (secret-generate-password))
      (kill-new pass)))
  ;; append-to-file doesn't work with epa (Emacs 24.1) so do append manually
  (with-current-buffer secret-password-file
    (goto-char (point-max))
    (insert (concat "\n" key "\t" pass))
    (save-buffer))
  (add-to-list 'secret-password-keys key))

(defun secret-update-password (key pass)
  "Update the password for this key. If password is nil or empty
string then autogenerate a password, and put it in the kill ring
after adding it to the secret file."
  (interactive
   (list (ido-completing-read "Key or site: " secret-password-keys)
         (read-input "New password (empty for auto): ")))
  (unless (member key secret-password-keys)
    (error "This key does not have a password to update"))
  (when (or (not pass) (equal "" pass))
    (progn
      (setq pass (secret-generate-password))
      (kill-new pass)))
  (with-current-buffer secret-password-file
    (goto-char (point-min))
    (re-search-forward (concat "^" key))
    (kill-whole-line)
    (insert (concat key "\t" pass "\n"))
    (save-buffer))
  )

(defun secret-generate-password ()
  (interactive)
  (substring (shell-command-to-string secret-generate-password-command) 0 -1))

(provide 'simple-secrets)

;;; simple-secrets.el ends here
