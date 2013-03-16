;; Copied from Ingo Lohmar
;; Fills according to sentences (ignoring fill-column): Whenever there is
;; a new sentence, it inserts a line break
(defun fill-sentence ()
  "Fill the current paragraph, separating sentences w/ a newline.

AUCTeX's latex.el reimplements the fill functions and is *very*
convoluted. We use part of it --- skip comment par we are in."
  (interactive)
  (if (save-excursion
        (beginning-of-line) (looking-at TeX-comment-start-regexp))
      (TeX-comment-forward)
  (let ((to (progn
              (LaTeX-forward-paragraph)
              (point)))
        (from (progn
                (LaTeX-backward-paragraph)
                (point)))
        (to-marker (make-marker)))
    (set-marker to-marker to)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      (setq tmp-end (point))
      (LaTeX-fill-region-as-paragraph from (- tmp-end -1))
      (setq from (point))
      (unless (bolp)
        (LaTeX-newline))))))
