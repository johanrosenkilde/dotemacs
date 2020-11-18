;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CURRENTLY DISABLED MODES
;;
;; OBS: THIS FILE IS NOT LOADED AT STARTUP!
;; It's meant as a place to park setup that I no longer care about, but which
;; may prove useful in the future
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Terminal
(autoload 'get-term "terminal_setup.el" "Fire up a terminal" t)
(global-set-key [(f9)] 'get-term)

;; Anki
(autoload 'anki-mode "anki_setup.el" "Major mode for writing Anki word lists" t)

;; Asymptote
(add-to-list 'load-path "/usr/share/texmf-dist/asymptote/")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
(defun jsrn-asy-mode-hook ()
  (interactive)
  (setq ps-view-command "okular"
        asy-command "asy -V -psviewer=okular" )
  (fill-keymap asy-mode-map
               (kbd "C-c C-h") 'asy-show-function-at-point)
  )
(add-hook 'asy-mode-hook 'jsrn-asy-mode-hook)
