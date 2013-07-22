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
;;; Commentary:
;; ToDo:support flinge
;; selective-display cannot support region

;;; Code:
(defcustom indent-folding-show-temp t
  nil
  :group 'indent-folding)

;; (setq indent-folding-show-temp t)

(defvar indent-folding-unfolded-overlays nil
  nil)
(make-variable-buffer-local 'indent-folding-unfolded-overlays)

(defvar indent-folding-post-folding nil)

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

(defun indent-folding-delete-overlay (overlay)
  (when (eq (overlay-get overlay 'face) 'indent-folding)
    ;; (with-silent-modifications
    ;;   (set-text-properties ...))
    (delete-overlay overlay)
    overlay))

(defun indent-folding-delete (overlay is-after begin end &optional len)
  (indent-folding-delete-overlay overlay))

(defun indent-folding-make-overlay (region)
  (let ((start (car region))
        (end (nth 1 region)))
    (when (< (line-number-at-pos start)
             (line-number-at-pos end))
      (let ((overlay (make-overlay start end)))
        ;; (- end 1)
        ;; 1- for exclude close blacket
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'isearch-open-invisible 'indent-folding-delete-overlay)
        (overlay-put overlay 'display "...")
        (overlay-put overlay 'face 'indent-folding)
        (overlay-put overlay 'insert-in-front-hooks '(indent-folding-delete))
        (overlay-put overlay 'insert-behind-hooks '(indent-folding-delete))
        (overlay-put overlay 'modification-hooks '(indent-folding-delete))
        (setq indent-folding-post-folding t)
        )
      )))

(defface indent-folding
  nil "Face for indent-folding"
  :group 'indent-folding)
;; todo darkcolor
;; '((t (:foreground "white" :background "red"))) nil)
;;   '((t (:background "white"))) nil)

(defun indent-folding-temp-show ()
  (when indent-folding-unfolded-overlays
    (mapc
     (lambda (overlay)
       (when (eq (overlay-get overlay 'face) 'indent-folding)
         (overlay-put overlay 'display "...")
         overlay))
     indent-folding-unfolded-overlays))
  (if indent-folding-post-folding
      (setq indent-folding-post-folding nil)
    (setq indent-folding-unfolded-overlays
          (delq nil
                (mapcar
                 (lambda (overlay)
                   (when (eq (overlay-get overlay 'face) 'indent-folding)
                     (overlay-put overlay 'display nil)
                     overlay))
                 (overlays-at (point)))))))

(defun indent-folding-region (start end)
  (interactive "r\nP")
  (let ((has-child nil))
    (save-excursion
      (goto-char start)
      ;; (forward-to-indentation)
      (let* ((next-region (indent-folding-forward-indentation))
             (next-start (nth 1 next-region)))
        (while (and (not (eobp)) (<= next-start end))
          (when (indent-folding-make-overlay next-region)
            (setq has-child t))
          (goto-char (1+ next-start))
          (setq next-region (indent-folding-forward-indentation))
          (setq next-start (nth 1 next-region))
          ))
    has-child
    )))

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
    (when overlays (mapc 'indent-folding-delete-overlay overlays))))

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
      (mapc 'indent-folding-delete-overlay overlays)
      (if (indent-folding-region (1+ start) end)
          (message "CHILDREN" )
        (message "SUBTREE (NO CHILDREN)")
        )
      )
     (t (mapc 'indent-folding-delete-overlay overlays)
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
;;;###autoload
(define-minor-mode indent-folding-mode
  "Toggle indent-folding mode"
  :keymap (make-sparse-keymap)
  (if indent-folding-mode indent-folding-show-temp
    (add-hook 'post-command-hook 'indent-folding-temp-show)
    (progn
      (remove-hook 'post-command-hook 'indent-folding-temp-show)
      (indent-folding-show-all)
      (setq indent-folding-unfolded-overlays nil)
      (setq indent-folding-post-folding nil)
      )))

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

;; (define-key indent-folding-mode-map "b" 'self-insert-command)
(define-key indent-folding-mode-map (kbd "TAB") 'indent-folding)
(define-key indent-folding-mode-map [BACKTAB] 'indent-folding-all)
(define-key indent-folding-mode-map [S-tab] 'indent-folding-all)

(provide 'indent-folding)
;;; indent-folding.el ends here
