(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-arg-item-label-p nil)
 '(appt-display-format (quote window))
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(fill-column 80)
 '(flyspell-auto-correct-binding nil)
 '(flyspell-highlight-flag t)
 '(inferior-sage-prompt "^\\(?:\\(?:\\(?:(\\(?:[Pg]db)\\)\\|\\.\\.\\.\\(?:\\.\\.\\)?\\|>>>\\|ipdb>\\|SAGE:\\)\\) \\)+")
 '(nil nil t)
 '(org-agenda-files (quote ("/home/jsrn/orgs/work.org" "/home/jsrn/orgs/home.org")))
 '(org-deadline-warning-days 7)
 '(preview-gs-options (quote ("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(preview-image-type (quote dvipng))
 '(preview-scale-function 1)
 '(sage-view-anti-aliasing-level 4)
 '(sage-view-latex-head "\\documentclass{article}
\\usepackage[active, tightpage, pdftex, displaymath]{preview}
\\usepackage{amstext}
\\begin{document}
\\begin{preview}
\\begin{math}")
 '(sage-view-scale 1.0)
 '(sage-view-scale-factor 1)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground
                         "black" :inverse-video nil :box nil :strike-through nil
                         :overline nil :underline nil :slant normal :weight
                         normal :height 78 :width normal :foundry "unknown"
                         :family "Droid Sans Mono"))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t)))))
