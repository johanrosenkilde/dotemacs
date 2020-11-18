(require 'hideshow)
(setq hs-isearch-open t) ; search in both code and comments
(setq py-hide-show-minor-mode-p t) 

;; Highlight folded regions better, and add number of lines folded
(defun hs-overlay-style (ov)
  (overlay-put ov 'face '(background-color . "#ffdfd2"))
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
                 (propertize
                  (format " ... <%d>"
                          (count-lines (overlay-start ov)
                                       (overlay-end ov)))
                  'face 'font-lock-type-face))))
(setq hs-set-up-overlay 'hs-overlay-style)

(setq jsrn-hs-hiding-all nil)
(defun hs-toggle-all ()
  (interactive)
  (if (not (local-variable-p jsrn-hs-hiding-all))
      (defvar-local jsrn-hs-hiding-all nil))
  (if jsrn-hs-hiding-all
      (progn
        (setq jsrn-hs-hiding-all nil)
        (hs-show-all))
    (progn
        (setq jsrn-hs-hiding-all t)
        (hs-hide-all)))
  )
(fill-keymap hs-minor-mode-map
             (kbd "C-<tab>" ) 'hs-toggle-hiding
             (kbd "M-C-<tab>" ) 'hs-toggle-all
             )
