;; Going to do interpolation highlighting just with font-lock-defaults and
;; font-lock-extend-region-functions. This means we need to keep track of where
;; the strings are in the buffer.

(setq interpolation-highlights
      '(("Sin\\|Cos\\|Sum" . font-lock-function-name-face)
        ("Pi\\|Infinity" . font-lock-constant-face)
        ("^FOO"
         (0 font-lock-keyword-face)  ;; subexp-highlighter - face for FOO
         (".*\\(\n.+\\)*BAR" (my-end-of-paragraph-position) nil (0 font-lock-variable-name-face)))))

(defun build-string-delimiters ()
  ;; Note we're not attempting to deal with comments here. Doing so will add a
  ;; decent amount of complexity

  (let ((string-delimiters ()))
   (save-excursion
     (goto-char (point-min))
     (while (search-forward "\"" nil t)
       (setq string-delimiters (cons (point) top-level-strings)))
     (reverse string-delimiters))))

(defun update-string-delimiters (change-region-begin
                                 change-region-end
                                 original-region-length)
  ;; (message "hook args: change-region-begin = %d, change-region-end = %d, original-region-length = %d" change-region-begin change-region-end original-region-length)
  ;; (message "string-delimiters: %s" top-level-strings)
  (let* ((next-delimiters string-delimiters)
         (current-delimiter-pos (car next-delimiters)))
    (while (and next-delimiters
                (< current-delimeter-pos change-region-begin))
      (setq next-delimiters (cdr next-delimiters))
      (setq current_delimiter-pos (car nexd-delimiters)))
    (when remaining-string-pairs
       ))
  )

(define-derived-mode interpolation-mode fundamental-mode "interpolation"
  "major mode for getting interpolation highlighting to work."
  (setq-local string-delimiters (build-top-level-strings))
  (message "%s" string-delimiters)
  ;; (setq font-lock-defaults '(interpolation-highlights))
  ;; (setq font-lock-multiline t)  Is this on by default?
  ;; (add-hook 'font-lock-extend-region-functions 'extend-region-to-paragraph)
  (add-hook 'after-change-functions 'update-string-delimiters nil t)
  )

