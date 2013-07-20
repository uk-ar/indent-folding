;;; indent-folding.el --- folding based on indentation.

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2013 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; ToDo:support flinge
;; selective-display cannot support region

;;;###autoload
(define-minor-mode indent-folding-mode
  "Toggle indent-folding mode"
  :keymap (make-sparse-keymap)
  )

(defcustom indent-folding-disable-modes '(org-mode)
  "Major modes `indent-folding-mode' can not run on."
  :group 'indent-folding)

(defun indent-folding-mode-maybe ()
  "What buffer `indent-folding-mode' prefers."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode indent-folding-disable-modes))
             ;; minor-mode
             (indent-folding-mode 1)
             )))

;;;###autoload
(define-global-minor-mode global-indent-folding-mode
  indent-folding-mode
  indent-folding-mode-maybe ;turn on
  ;; :init-value t bug?
  :group 'indent-folding)

;; setup
(global-indent-folding-mode t)

(defun indent-folding-forward-indentation (&optional column)
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
                      (looking-at "[ \t]*$"))
                  )
        )
      `(,(min start (point-at-eol 0)) ,(max start (point-at-eol 0)))
      )
    ))

(defun indent-folding-make-overlay (region)
  (let ((start (car region))
        (end (nth 1 region)))
    (when (< (line-number-at-pos start)
             (line-number-at-pos end))
      (let ((overlay (make-overlay start end)))
        ;; (- end 1)
        ;; 1- for exclude close blacket
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'isearch-open-invisible 'delete-overlay)
        (overlay-put overlay 'before-string "...")
        (overlay-put overlay 'face 'indent-folding)
        )
      )))

(defface indent-folding nil
  "Face for indent-folding"
  );'((t (:background "white"))) nil)

(defun indent-folding-region (start end)
  (interactive "r\nP")
  (let ((has-child nil))
    (save-excursion
      (goto-char start)
      (forward-to-indentation)
      (let* ((next-region (indent-folding-forward-indentation))
             (next-start (nth 1 next-region)))
        (while (and (not (eobp)) (<= next-start end))
          (when (indent-folding-make-overlay next-region)
            (setq has-child t))
          (goto-char (1+ next-start))
          (setq next-region (indent-folding-forward-indentation))
          (setq next-start (nth 1 next-region))
          )))
    has-child
    ))

(defun indent-folding-overlays-in (start end)
  (delq nil
        (mapcar
         (lambda (overlay)
           (when (eq (overlay-get overlay 'face) 'indent-folding)
             overlay))
         (overlays-in start end))))

(defun indent-folding-show-all ()
  (interactive)
  (let ((overlays (indent-folding-overlays-in (point-min) (point-max))))
    (when overlays (mapc 'delete-overlay overlays))))

(defun indent-folding-hide-all ()
  (interactive)
  (indent-folding-region (point-min) (point-max)))

(defun indent-folding-all ()
  (interactive)
  (if (indent-folding-overlays-in (point-min) (point-max))
      (indent-folding-show-all)
    (indent-folding-hide-all)
    ))

(defun indent-folding-1 ()
  (let* ((region (indent-folding-forward-indentation))
         (start (car region))
         (end (nth 1 region))
         (overlays (indent-folding-overlays-in start end)))
    (cond
     ((null overlays)
      (when (indent-folding-make-overlay region)
        (message "FOLDED"))
      )
     ((and (= (length overlays) 1)
           (= (overlay-start (car overlays)) start)
           (= (overlay-end (car overlays)) end)
           )
      (mapc 'delete-overlay overlays)
      (if (indent-folding-region start end)
          (message "CHILDREN" )
        (message "SUBTREE (NO CHILDREN)")
        )
      )
     (t (mapc 'delete-overlay overlays)
        (message "SUBTREE")
        ;; (message "SUBTREE (NO CHILDREN)")
        ))))

(defun indent-folding ()
  (interactive)
  (let* ((last-point (point))
         (indent-folding-mode nil)
         (command (key-binding (this-command-keys-vector)))
         (region (region-active-p));; for indent removes mark
         )
    (when (commandp command)
      (call-interactively command))
    (when (and (not region)
               (equal last-point (point)))
      (indent-folding-1))
    ))

;; (define-key indent-folding-mode-map "b" 'self-insert-command)
(define-key indent-folding-mode-map (kbd "TAB") 'indent-folding)
(define-key indent-folding-mode-map [BACKTAB] 'indent-folding-all)
(define-key indent-folding-mode-map [S-tab] 'indent-folding-all)

(provide 'indent-folding)
;;; indent-folding.el ends here
