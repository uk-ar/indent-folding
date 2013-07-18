;; selective-display cannot support region
;; (set-selective-display 3)
;; (set-selective-display 1)
;; (set-selective-display 10)
(define-minor-mode yafolding-mode
  "Toggle yafolding mode"
  :init-value t
  :keymap (make-sparse-keymap)

  )

(defun yafolding-forward-indentation (&optional column)
  (save-excursion
    ;; {
    ;;   //
    ;; }
    ;; style?
    (let ((start (point-at-eol))
          (indent
           (or column
               (progn (back-to-indentation)
                      (current-column)))))
      (while (and (not (eobp))
                  (or (< indent (forward-to-indentation))
                      ;; copy from delete-blank-lines
                      (looking-at "^[ \t]*$"))
                  )
        )
      `(,start ,(point-at-eol 0))
      )
    ))

(mapc
 'delete-overlay
 (overlays-in (point-min) (point-max)))

(defun yafolding2-make-overlay (region)
  (let ((start (car region))
        (end (nth 1 region)))
    (when (< (line-number-at-pos start)
             (line-number-at-pos end))
      (let ((overlay (make-overlay start end)))
        ;; (- end 1)
        ;; 1- for exclude close blacket
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'isearch-open-invisible-temporary t)
        (overlay-put overlay 'before-string "...")
        (overlay-put overlay 'face 'yafolding2)
        (message "FOLDED")
        )
      )))

(defface yafolding2 '((t (:background "white"))) nil)
(defun yafolding2 ()
  (let* ((region (yafolding-forward-indentation))
         (start (car region))
         (end (nth 1 region))
         (overlays (delq nil
                         (mapcar
                          (lambda (overlay)
                            (when (eq (overlay-get overlay 'face) 'yafolding2)
                              overlay))
                          (overlays-in start end)))))
    (cond
     ((null overlays)
      (yafolding2-make-overlay region)
      )
     ((and (= (length overlays) 1)
           (= (overlay-start (car overlays)) start)
           (= (overlay-end (car overlays)) end)
           )
      (mapc 'delete-overlay overlays)
      (let ((next-column (1+ (current-column)))
            (done nil));; change to cl loop
        (save-excursion
          (let* ((next-region (yafolding-forward-indentation next-column))
                 (next-start (nth 1 next-region)))
          (while (and (not done) (not (equal next-region region)))
            ;; (let* ((next-region (yafolding-forward-indentation next-column))
            ;;        (next-start (nth 1 next-region))
            ;;        )
              (yafolding2-make-overlay next-region)
              (if (< next-start end)
                  (progn
                    (goto-char (1+ next-start))
                    (setq next-region (yafolding-forward-indentation next-column))
                    (setq next-start (nth 1 next-region))
                    )
                (progn
                  (setq done t)
                  )
                )
              )
            ))
        )
      (message "CHILDREN" )
      )
     (t (mapc 'delete-overlay overlays)
        (message "SUBTREE")
        ;; (message "SUBTREE (NO CHILDREN)")
        ))))
;; (forward-to-indentation)

(defun yafolding-trigger ()
  (interactive)
  (let* ((last-point (point))
         (yafolding-mode nil)
         (command (key-binding (this-command-keys-vector))))
    (when (commandp command)
      (call-interactively command))
    (when (and (equal last-point (point))
               (not mark-active))
      (yafolding2))
    ))

;; (overlay-properties (car (overlays-in (point-min) (point-max))))
;; (set-selective-display 10)
;; (delete-overlay (overlays-at (point)))
;; (current-column)
;; (back-to-indentation)
;; (forward-to-indentation)

;; (define-key yafolding-mode-map "b" 'self-insert-command)
(define-key yafolding-mode-map (kbd "TAB") 'yafolding-trigger)
(define-key yafolding-mode-map [BACKTAB] 'yafolding-trigger)
(define-key yafolding-mode-map [S-tab] 'yafolding-trigger)
