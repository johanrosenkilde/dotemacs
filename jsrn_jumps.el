;; jsrn_jumps.el
;; This is exhumed jumping behaviour from Evil before commit
;;    4217737: "replace existing jump list functionality with evil-jumper"
;;
;; This replaces the new Evil jump semantics with the previous per-buffer,
;; per-window jumping, i.e. jumping backward doesn't change the buffer or
;; window.

(setq jsrn-evil-jump-list nil)

(evil-define-motion jsrn-evil-jump-backward (count)
  "Go to older position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-forward]."
  (let ((current-pos (make-marker))
        (count (or count 1)) i)
    (unless jsrn-evil-jump-list
      (move-marker current-pos (point))
      (add-to-list 'jsrn-evil-jump-list current-pos))
    (evil-motion-loop (nil count)
      (setq current-pos (make-marker))
      ;; skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (set-mark-command 0)
                    (setq i (1- i))
                    (and (= (point) current-pos) (> i 0))))
      ;; Already there?
      (move-marker current-pos (point))
      (unless (= current-pos (car-safe jsrn-evil-jump-list))
        (add-to-list 'jsrn-evil-jump-list current-pos)))))

(evil-define-motion jsrn-evil-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[jsrn-evil-jump-backward]."
  (let ((count (or count 1))
        current-pos next-pos)
    (evil-motion-loop (nil count)
      (setq current-pos (car-safe jsrn-evil-jump-list)
            next-pos (car (cdr-safe jsrn-evil-jump-list)))
      (when next-pos
        (push-mark current-pos t nil)
        (unless (eq (marker-buffer next-pos) (current-buffer))
          t)
        (goto-char next-pos)
        (pop jsrn-evil-jump-list)))))

(defun jsrn-evil-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (unless (or (region-active-p) (evil-visual-state-p))
    (evil-save-echo-area
      (mapc #'(lambda (marker)
                (set-marker marker nil))
            jsrn-evil-jump-list)
      (setq jsrn-evil-jump-list nil)
      (push-mark pos t))))

(message "Loaded jsrn_jumps.el")
(provide 'jsrn_jumps)
