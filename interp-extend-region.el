;; -*- lexical-binding: t; -*-

;; Going to do interpolation highlighting just with font-lock-defaults and
;; font-lock-extend-region-functions. This means we need to keep track of where
;; the strings are in the buffer.

(setq interpolation-highlights
      '(("Sin\\|Cos\\|Sum" . font-lock-function-name-face)
        ("Pi\\|Infinity" . font-lock-constant-face)
        ("^FOO"
         (0 font-lock-keyword-face)  ;; subexp-highlighter - face for FOO
         (".*\\(\n.+\\)*BAR" (my-end-of-paragraph-position) nil (0 font-lock-variable-name-face)))))

(defun delimiters-in-region (start end)
  ;; We store the point immediately after the `"' in the delimiter list.
  ;; Note we're not attempting to deal with comments here. Doing so will add a
  ;; decent amount of complexity
  (let ((string-delimiters ()))
    (save-excursion
      (goto-char start)
      (while (and (search-forward "\"" nil t) (<= (point) end))
        (setq string-delimiters (cons (point) string-delimiters)))
      (reverse string-delimiters))))

(defun update-string-delimiters (change-region-begin
                                 change-region-end
                                 original-region-length)
  (message "hook args: change-region-begin = %d, change-region-end = %d, original-region-length = %d" change-region-begin change-region-end original-region-length)
  (message "HOOK START, string-delimiters: %s" string-delimiters)
  (setq string-delimiters (delimiters-update string-delimiters
                                             change-region-begin
                                             change-region-end
                                             original-region-length))
  (message "HOOK END,   string-delimiters: %s" string-delimiters))

(defun delimiters-update (delimiters
                          change-region-begin
                          change-region-end
                          original-region-length)
  (let* ((original-region-end (+ change-region-begin original-region-length))
         (delimiters-before-region (delimiters-before-region delimiters
                                                             change-region-begin))
         (delimiters-in-region (delimiters-in-region change-region-begin change-region-end))
         (dirty-delimiters-after-region (delimiters-after-region delimiters
                                                                 original-region-end))
         (new-region-length (- change-region-end change-region-begin))
         (pos-adjustment (- new-region-length original-region-length))
         (delimiters-after-region (seq-map (lambda (pos) (+ pos pos-adjustment))
                                           dirty-delimiters-after-region)))
    (seq-concatenate 'list
                     delimiters-before-region
                     delimiters-in-region
                     delimiters-after-region)))

(defun delimiters-before-region (delimiters region-begin)
  (seq-take-while (lambda (pos) (<= pos region-begin)) delimiters))

(defun delimiters-after-region (delimiters region-end)
  (seq-drop-while (lambda (pos) (<= pos region-end)) delimiters))

(define-derived-mode interpolation-mode fundamental-mode "interpolation"
  "major mode for getting interpolation highlighting to work."
  (setq-local string-delimiters (delimiters-in-region (point-min) (point-max)))
  (message "%s" string-delimiters)
  ;; (setq font-lock-defaults '(interpolation-highlights))
  ;; (setq font-lock-multiline t)  Is this on by default?
  ;; (add-hook 'font-lock-extend-region-functions 'extend-region-to-paragraph)
  (add-hook 'after-change-functions 'update-string-delimiters nil t))
