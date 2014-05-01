(setq zathura-procs ())
(defun zathura-forward-search ()
  ;; Open the compiled pdf in Zathura with synctex. This is complicated since
  ;; 1) Zathura refuses to acknowledge Synctex directive if the pdf is not
  ;; already opened
  ;; 2) This means we have to bookkeep open Zathura processes ourselves: first
  ;; open a new pdf from the beginning, if it is not already open. Then call
  ;; Zathura again with the synctex directive.
  ;; TODO: This currently doesn't work:
  ;; - The pdf is correctly opened.
  ;; - Zathura correctly updates it's position
  ;; - But the Zathura window doesn't steal the focus (also not when called from
  ;;   shell)
  ;; Backward search is still completely untested, but the following argument to
  ;; zathura should work
  ;; -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\""
  (interactive)
  (let* ((pdfname (concat (file-name-sans-extension (TeX-master-file))
                          ".pdf"))
         (zatentry (assoc pdfname zathura-procs))
         (zatproc (if (and zatentry (process-live-p (cdr zatentry)))
                      (cdr zatentry)
                    (progn
                      (let ((proc (start-process "zathura" nil "zathura" "-s" pdfname)))
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
    ;;TODO: the monkey buffer is for debugging to check that Zathura doesn't
    ;;output an error
    (start-process "zathura" (get-buffer-create "monkey") "zathura" "--synctex-forward" synctex pdfname)
    ))

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
(setq LaTeX-math-list '((?o "ell" nil)))
;; For spelling, add the cref commands to the "don't check contents" list
(setq flyspell-tex-command-regexp
  "\\(\\(begin\\|end\\)[ \t]*{\\|\\(cite[a-z*]*\\|label\\|c?\\(page\\)?ref\\|eqref\\|usepackage\\|documentclass\\)[ \t]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
;; Load reftex
(require 'bibtex)
(require 'reftex)
(defun jsrn-latex-mode-hook ()
  (local-set-key (kbd "M-q") 'fill-sentence)  ; hard sentence wrap
  (setq fill-column 9999)            ; with hard senctence wrap, we don't want hard lines
  (visual-line-mode t)               ; but we do want visual word wrap
  (adaptive-wrap-prefix-mode t)      ; with adaptive indenting
  (LaTeX-math-mode t)                ; always turn on math mode
  (flyspell-mode t)                  ; always turn on flyspell
  (turn-on-reftex)                   ; always turn on reftex
  (setq TeX-insert-braces nil        ; dont ever insert braces at macro expansion
        TeX-arg-item-label-p nil     ; dont ask for labels on all commands
        LaTeX-item-indent 0          ; indent \item as other stuff inside envs (works
                                     ; better with adaptive-wrap-prefix-mode)
        )
  ;; Activate the more reliable but simpler error system (for C-c `)
  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
  (TeX-source-correlate-mode)        ; activate forward/reverse search
  (TeX-PDF-mode)
  ;; Make forward search work properly for Okular
  (defun okular-make-url ()
    (concat "file://"
            (expand-file-name (funcall file (TeX-output-extension) t)
                              (file-name-directory (TeX-master-file)))
            "#src:"
            (TeX-current-line)
            default-directory
            (TeX-current-file-name-master-relative)))
  (add-to-list 'TeX-expand-list '("%u" okular-make-url)) ;; Expand %u to the result of above fun
  (setq TeX-view-program-list '(("okular" "okular --unique %u")))
  (setq TeX-view-program-selection (quote ((output-pdf "okular") (output-dvi "xdvi"))))
  ;; TODO: To open pdfs with Zathura, if zathura-forward-search can be made to work.
  ;; (add-to-list 'TeX-view-program-list
  ;;              '("zathura" zathura-forward-search))
  ;; (setq TeX-view-program-selection (quote ((output-pdf "zathura") (output-dvi "xdvi"))))
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
  ;; Don't prompt for ref style, just insert cref always
  (setq reftex-ref-macro-prompt nil)
  ;; Don't put stupid parentheses around refs to equations.
  ;;NOTE: overridden by next line
  ;;(setq reftex-label-alist (list '("equation" 101 "eqn:" "~\\ref{%s}" t)))
  ;; Insert eqref for equations
  (setq reftex-label-alist (list '("equation" 101 "eqn:" "~\\eqref{%s}" t)))
  ;; The above seems to be broken in current AUCTeX so work-around
  (fill-keymaps (list evil-normal-state-map evil-insert-state-map)
                (kbd "C-c )") (lambda () (interactive) (reftex-reference " ")))
  (add-to-list 'reftex-ref-style-alist '("Default" t
                                         (("\\cref" 13)
                                          ("\\cpageref" 112))))
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
  (define-key evil-normal-state-map (kbd "M-#") 'goto-current-label) 

  ;; Disable opening the reftex toc on C-c - as well
  ;;TODO: This doesn't work since the keybinding keeps getting redefined by reftex
  (define-key evil-normal-state-map (kbd "C-c - ") '(lambda () (interactive) t))

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

  ;; Let M-q fill entire entries and not just single items
  (setq fill-paragraph-function (lambda (&optional args) (bibtex-fill-entry)))
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
  )
(add-hook 'LaTeX-mode-hook 'jsrn-latex-mode-hook)

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
