;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ELISP UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookup (key alist default)
  "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))  

(defun pour-mappings-to (map mappings)
  "Calls `define-key' with `map' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a define-key-understandable string and a interactive-fun."
  (dolist (mapping (-partition 2 mappings))
    (define-key map (car mapping) (cadr mapping)))
  map)

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (pour-mappings-to keymap mappings))

(defun fill-keymaps (keymaps &rest mappings)
  "Fill `KEYMAPS' with `MAPPINGS'.
See `pour-mappings-to'."
  (dolist (keymap keymaps keymaps)
    (let ((map (if (symbolp keymap)
                   (symbol-value keymap)
                 keymap)))
      (pour-mappings-to map mappings))))

(defun key-binding-other-window (keyb)
  "Run the given key binding in the other window"
  (interactive)
  (other-window 1)
  (condition-case err
      (funcall (key-binding keyb))
    (error (princ (format "Error: %s" err))))
  (other-window -1))

(defun next-in-list (ls obj)
  "Find the element in ls which is after obj. Returns the first
element of ls if obj is not in ls or is the last."
  (let* ((inlist (member obj ls)))
    (if (and inlist (cdr inlist))
         (car (cdr inlist))
       (car ls))))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))
