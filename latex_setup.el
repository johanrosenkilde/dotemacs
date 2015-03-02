;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Emacs-wide loads, vars etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bibtex)
(require 'reftex)
(diminish 'reftex-mode)
(diminish 'outline-minor-mode)


;; Setup Zathura
(setq zathura-procs ())
(defun zathura-forward-search ()
  ;; Open the compiled pdf in Zathura with synctex. This is complicated since
  ;; 1) Zathura refuses to acknowledge Synctex directive if the pdf is not
  ;; already opened
  ;; 2) This means we have to bookkeep open Zathura processes ourselves: first
  ;; open a new pdf from the beginning, if it is not already open. Then call
  ;; Zathura again with the synctex directive.
  (interactive)
  (let* ((zathura-launch-buf (get-buffer-create "*Zathura Output*"))
         (pdfname (TeX-master-file "pdf"))
         (zatentry (assoc pdfname zathura-procs))
         (zatproc (if (and zatentry (process-live-p (cdr zatentry)))
                      (cdr zatentry)
                    (progn
                      (let ((proc (progn (message "Launching Zathura")
                                         (start-process "zathura-launch"
                                                        zathura-launch-buf "zathura"
                                                         "-x" "emacsclient +%{line} %{input}" pdfname))))
                        (when zatentry
                          (setq zathura-procs (delq zatentry zathura-procs)))
                        (add-to-list 'zathura-procs (cons pdfname proc))
                        (set-process-query-on-exit-flag proc nil)
                        proc))))
         (pid (process-id zatproc))
         (synctex (format "%s:0:%s"
                          (TeX-current-line)
                          (TeX-current-file-name-master-relative)))
         )
    (start-process "zathura-synctex" zathura-launch-buf "zathura" "--synctex-forward" synctex pdfname)
    (start-process "raise-zathura-wmctrl" zathura-launch-buf "wmctrl" "-a" pdfname)
    ))

;; Search functions
(defun search-in-math (regex)
  "Search for a given regular expression only in math mode."
  (interactive "sSearch term: ")
  (setq search-in-math-last regex)
  (search-in-math-repeat)
  )

(defun search-in-math-repeat ()
  "Search for the next occurence of an expression only in math mode.
Uses last value searched for in math mode."
  (interactive)
  (let ((case-fold-search nil)) ;; math is always case sensitive
    (when (not (search-forward-regexp search-in-math-last nil 0))
      (message "Searched failed from search point and onwards."))
    (while (not (or (texmathp) (eq (point) (point-max))))
      (search-forward-regexp search-in-math-last))
    (match-beginning 0)
    )
  )

;; Function for jumping according to labels
(defun reftex-goto-named-label (label)
  "Take a latex label and goto the definition of it without prompting and in the
same window.
This is a modified version of reftex-goto-label from 24.3.1"
  (reftex-access-scan-info)
  (message label)
  (let* ((wcfg (current-window-configuration))
         (docstruct (symbol-value reftex-docstruct-symbol))
         (selection (assoc label docstruct))
         (where (progn
                  (reftex-show-label-location selection t nil 'stay)
                  (point-marker))))
    (set-window-configuration wcfg)
    (switch-to-buffer (marker-buffer where))
    (goto-char where)
    (reftex-unhighlight 0))
  )
(defun goto-current-label ()
  "If the cursor is on top of a (complex) reference, guess the label and goto the definition"
  (interactive)
  (let ((refcall (thing-at-point 'filename)))
                                      ;(message "'%s'" refcall)
                                      ;(if (string-match "vreflem{wu_params}" refcall)
    (if (string-match "c?\\(v\\|page\\|\\)ref\\([^{]*\\){\\([^}]*\\)}" refcall)
        (let ((reftype (match-string 2 refcall))
              (label (match-string 3 refcall)))
          (progn
            (message "'%s' '%s'" reftype label)
            (evil-set-jump) ; save current point before jumping
            (if (and (not (string-equal reftype "")) (not (string-equal reftype "page")))
                (reftex-goto-named-label (cl-concatenate 'string reftype ":" label))
              (reftex-goto-named-label label))
            ))
      (message "not a valid label: '%s'" refcall)
      )))

;; Set evil jump when using Latex's jumping stuff
(defadvice LaTeX-mark-environment (before latex-mark-env-set-jump activate)
  "Store current position in jump list"
  (evil-set-jump)
  (evil-set-jump) ;; it seems the last registered jump will be overwritten
  )
(defadvice LaTeX-mark-section (before latex-mark-sec-set-jump activate)
  "Store current position in jump list"
  (evil-set-jump)
  (evil-set-jump))

;; Fill sentence: reflows paragraph to have only linebreaks at sentence boundaries
(load "fill-sentence.el")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-parse-self nil)
;; Add "% <something> AREA" and "\frontchapter" as outline headers
(setq TeX-outline-extra '(("%.* AREA" 1)
                          ("\\\\frontchapter" 1)))
;; Add custom reference commands
(setq font-latex-match-reference-keywords '(("cref" "{")
                                            ("cpageref" "{")
                                            ))
;; Add some shortcuts in math mode
;; NOTE: Don't forget to call `LaTeX-math-initalize' afterwards
(setq LaTeX-math-list (list  '(? "ldots" nil)
                             '(?o "ell" nil)
                             '("v v" "vec" nil)
                             '(?9 "vec" nil)
                             '(?1 "hat" nil)
                             '(?2 "deg" nil)
                             ))
(LaTeX-math-initialize)

;; For spelling, add the cref commands to the "don't check contents" list
(setq flyspell-tex-command-regexp
  "\\(\\(begin\\|end\\)[ \t]*{\\|\\(cite[a-z*]*\\|label\\|c?\\(page\\)?ref\\|eqref\\|usepackage\\|documentclass\\)[ \t]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")

(setq TeX-insert-braces nil        ; dont ever insert braces at macro expansion
      TeX-arg-item-label-p nil     ; dont ask for labels on all commands
      LaTeX-item-indent 0          ; indent \item as other stuff inside envs (works
                                   ; better with adaptive-wrap-prefix-mode)
      )
;; Activate the more reliable but simpler error system (for C-c `)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
(TeX-source-correlate-mode)        ; activate forward/reverse search
(TeX-PDF-mode)
(add-to-list 'TeX-view-program-list
             '("zathura" zathura-forward-search))
(setq TeX-view-program-selection (quote ((output-pdf "zathura") (output-dvi "xdvi"))))

;; Don't prompt for ref style, just insert cref always
(setq reftex-ref-macro-prompt nil)
;; Don't put stupid parentheses around refs to equations.
;;NOTE: overridden by next line
;;(setq reftex-label-alist (list '("equation" 101 "eqn:" "~\\ref{%s}" t)))
;; Insert eqref for equations
(setq reftex-label-alist (list '("equation" 101 "eqn:" "~\\eqref{%s}" t)))

(add-to-list 'reftex-ref-style-alist '("Default" t
                                       (("\\cref" 13)
                                        ("\\cpageref" 112))))

;; Teach AucTeX about align and IEEEeqnarray
(LaTeX-add-environments
 '("align" LaTeX-env-label)
 '("align*" LaTeX-env-label)
 '("IEEEeqnarray" LaTeX-env-label)
 '("IEEEeqnarray*" LaTeX-env-label))
(add-to-list 'font-latex-math-environments "align")
(add-to-list 'font-latex-math-environments "align*")
(add-to-list 'font-latex-math-environments "IEEEeqnarray")
(add-to-list 'font-latex-math-environments "IEEEeqnarray*")
(add-to-list 'reftex-label-alist '("IEEEeqnarray" 101 "eqn:" "~\\eqref{%s}" t))
(add-to-list 'reftex-label-alist '("IEEEeqnarray*" 101 "eqn:" "~\\eqref{%s}" t))
(add-to-list 'reftex-label-alist '("align" 101 "eqn:" "~\\eqref{%s}" t))
(setq texmathp-tex-commands '(("IEEEeqnarray" env-on) ("IEEEeqnarray*" env-on)))

(defun jsrn-reftex-select-bib-mode-hook ()
  ;; e is set locally by stupid reftex, kill it
  (local-unset-key "e")
  ;; In bibtex-selection, set 'e' to previous
  (fill-keymap reftex-select-bib-mode-map
               (kbd "e") 'reftex-select-previous)
  )
(add-hook 'reftex-select-bib-mode-hook 'jsrn-reftex-select-bib-mode-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for setting up each buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jsrn-latex-mode-hook ()
  (local-set-key (kbd "M-q") 'fill-sentence)  ; hard sentence wrap
  (setq fill-column 9999)            ; with hard senctence wrap, we don't want hard lines
  (visual-line-mode t)               ; but we do want visual word wrap
  (adaptive-wrap-prefix-mode t)      ; with adaptive indenting
  (LaTeX-math-mode t)                ; always turn on math mode
  (flyspell-mode t)                  ; always turn on flyspell
  (turn-on-reftex)                   ; always turn on reftex
  (electric-pair-mode)               ; insert matching braces
  (define-key LaTeX-mode-map (kbd "$") 'self-insert-command) ; makes electric pairs work for $

  ;;When inserting a label, just use cref and don't ask
  (local-set-key (kbd "C-c 0")
                 '(lambda ()
                    "Choose a label and insert the appropriate cref{...} for that label"
                    (interactive)
                    (let* ((label (reftex-reference " " t)))
                      (insert (format "\\cref{%s}" label)))
                    ))

  ;; The above seems to be broken in current AUCTeX so work-around
  (fill-keymaps (list evil-normal-state-map evil-insert-state-map)
                (kbd "C-c )") (lambda () (interactive) (reftex-reference " ")))

  (define-key evil-normal-state-map (kbd "M-#") 'goto-current-label) 

  ;; Disable opening the reftex toc on C-c - as well
  ;;TODO: This doesn't work since the keybinding keeps getting redefined by reftex
  (define-key evil-normal-state-map (kbd "C-c - ") '(lambda () (interactive) t))

  ;; Let M-q fill entire entries and not just single items
  (setq fill-paragraph-function (lambda (&optional args) (bibtex-fill-entry)))

  ;; Other key bindings
  (fill-keymap evil-normal-state-local-map
               (kbd "C-#") 'jsrn-goto-first-symbol-use
               )
  )
(add-hook 'LaTeX-mode-hook 'jsrn-latex-mode-hook)

;; This file is excecuted when a LaTeX buffer is opened, so the above hook is
;; not run for that file. Therefore, run the hook.
(jsrn-latex-mode-hook)

(defun jsrn-bibtex-mode-hook ()
  (fill-keymap bibtex-mode-map
               (kbd "M-[") 'backward-block
               (kbd "M-]") 'forward-block
               (kbd "C-j") 'yank-block
               )
  (fill-keymap evil-motion-state-local-map
               (kbd "M-[") 'backward-block
               (kbd "M-]") 'forward-block
               )
  )
(add-hook 'bibtex-mode-hook 'jsrn-bibtex-mode-hook)

(message "Loaded latex_setup.el")
(provide 'latex_setup)
