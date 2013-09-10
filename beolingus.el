(defun beolingus-lookup (word)
  (interactive "sLook up german word: ")
  (message "Downloading")
  (let ((url (concat
  "http://dict.tu-chemnitz.de/dings.cgi?lang=en&service=deen&opterrors=0&optpro=0&query="
  word "&iservice=&comment=&email="))
        (htmlfile (concat temporary-file-directory "beo_" word ".html"))
        (outbuffer "*beolingus*"))
    (if (not (file-exists-p htmlfile))
        (url-copy-file  url htmlfile))
    (shell-command (concat "vilistextum -w 10000 -u " htmlfile " -") outbuffer)
    (save-excursion
      (set-buffer outbuffer)
      (goto-char (point-min))
      ; Cut the beginning cruft
      (search-forward-regexp "[:digit:]* \\(similar \\)?results? for")
      (previous-line)
      (delete-region (point-min) (point))
      ; Kill link remainders and trim line beginnings
      (while (re-search-forward "\\[[^][]*\\]\\|\\[\\[[^[]*\\]\\]\\|^ +" nil t)
        (replace-match ""))
      ; Kill empty lines
      (goto-char (point-min))
      (delete-blank-lines)
      (while (not (eq (point) (point-max)))
        (forward-line)
        (delete-blank-lines))
      (goto-char (point-min))
      ;;Kill some cruft
      (forward-line)
      (kill-line) (kill-line) (kill-line) (kill-line)
      ;Find some end
      (let ((begin (point))
            (end (progn (forward-line 20) (point))))
        (table-capture begin end "   " "\n" "l" "40")
        )
      )
  ))
