(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-parse-self nil)
;; Add "% <something> AREA" and "\frontchapter" as outline headers
(setq TeX-outline-extra '(("%.* AREA" 1)
                          ("\\\\frontchapter" 1)))
;; Add custom reference commands
(setq font-latex-match-reference-keywords '(("reflem" "{")
                                            ("refthm" "{")
                                            ("refprop" "{")
                                            ("refcor" "{")
                                            ("refconj" "{")
                                            ("refquest" "{")
                                            ("refprob" "{")
                                            ("refdef" "{")
                                            ("refchp" "{")
                                            ("refsec" "{")
                                            ("refssec" "{")
                                            ("refalg" "{")
                                            ("reffig" "{")
                                            ("reftbl" "{")
                                            ("refex" "{")
                                            ("refline" "{")
                                            ("refeqn" "{")
                                            ("refinv" "{")
                                            ("refpage" "{")
                                            ("pageref" "{")
                                            ("vreflem" "{")
                                            ("vrefthm" "{")
                                            ("vrefprop" "{")
                                            ("vrefcor" "{")
                                            ("vrefconj" "{")
                                            ("vrefquest" "{")
                                            ("vrefprob" "{")
                                            ("vrefdef" "{")
                                            ("vrefalg" "{")
                                            ("vreffig" "{")
                                            ("vreftbl" "{")
                                            ("vrefex" "{")
                                            ("vrefeqn" "{")
                                            ))
;; Add some shortcuts in math mode
(setq LaTeX-math-list '((?o "ell" nil)))
;; For spelling, add the ref<something> commands to the "don't check contents" list
(setq flyspell-tex-command-regexp
  "\\(\\(begin\\|end\\)[ \t]*{\\|\\(cite[a-z*]*\\|label\\|ref[a-z]*\\|eqref\\|usepackage\\|documentclass\\)[ \t]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
;; Load reftex
(require 'bibtex)
(require 'reftex)
(defun jsrn-latex-mode-hook ()
  (local-set-key (kbd "M-q") 'fill-sentence)  ; hard sentence wrap
  (local-set-key [(f3)] 'TeX-view)   ; possibly open xdvi and goto line
  (setq fill-column 9999)            ; with hard senctence wrap, we don't want hard lines
  (visual-line-mode t)               ; but we do want visual word wrap
  (adaptive-wrap-prefix-mode t)      ; with adaptive indenting
  (setq LaTeX-item-indent 0)         ; indent \item as other stuff inside envs (works
                                        ; better with adaptive-wrap-prefix-mode)
  (LaTeX-math-mode t)                ; always turn on math mode
  (flyspell-mode t)                  ; always turn on flyspell
  (turn-on-reftex)                   ; always turn on reftex
  (setq TeX-insert-braces nil)       ; dont ever insert braces at macro expansion
  (setq TeX-source-correlate-method 'source-specials)  ;; auctex 10.86  
  (electric-pair-mode)               ; insert matching braces
  (define-key LaTeX-mode-map (kbd "$") 'self-insert-command) ; makes electric pairs work for $
  (TeX-source-correlate-mode t)

  ;; Toggle outline mode and add Org-like key-bindings
  (outline-minor-mode t) ; remember that it is diminished in diminish area
  (local-set-key (kbd "C-<tab>") 'outline-toggle-children)
  (setq jsrn-current-sublevels 1)
  (local-set-key (kbd "C-S-<tab>")
                 '(lambda ()
                    "Cycle through hiding levels 1, 2, or show all"
                    (interactive)
                    (setq jsrn-current-sublevels (+ (mod jsrn-current-sublevels 3) 1))
                    (if (eq jsrn-current-sublevels 1)
                        (show-all)
                      (hide-sublevels jsrn-current-sublevels))))
  (local-set-key (kbd "C-c 0")
                 '(lambda ()
                    "Choose a label and insert the appropriate ref*{...} for that label"
                    (interactive)
                    (let* ((label (reftex-reference " " t))
                           (parts (split-string label ":")))
                      (if (string-equal label (car parts))
                          ;; no colon, insert plain ref
                          (insert (format "\\ref{%s}" label))
                        ;; there was a colon, so insert the respective ref
                        (insert (format "\\ref%s{%s}" (car parts) (car (cdr parts))))
                          ))))

  ;; Make a function and keybinding for jumping to label under cursor
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
      (if (string-match "\\(v\\|page\\|\\)ref\\([^{]*\\){\\([^}]*\\)}" refcall)
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
  (define-key evil-normal-state-map (kbd "M-#") 'goto-current-label) 

  ;; Disable opening the reftex toc on C-c - as well
  ;;TODO: This doesn't work since the keybinding keeps getting redefined by reftex
  (define-key evil-normal-state-map (kbd "C-c - ") '(lambda () (interactive) t))

  ;; Teach AucTeX about IEEEeqnarray
  (LaTeX-add-environments
   '("IEEEeqnarray" LaTeX-env-label)
   '("IEEEeqnarray*" LaTeX-env-label))
  (add-to-list 'font-latex-math-environments "IEEEeqnarray")
  (add-to-list 'font-latex-math-environments "IEEEeqnarray*")
  (setq texmathp-tex-commands '(("IEEEeqnarray" env-on) ("IEEEeqnarray*" env-on)))
  )
(add-hook 'LaTeX-mode-hook 'jsrn-latex-mode-hook)
