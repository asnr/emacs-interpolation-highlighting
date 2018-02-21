;; -*- lexical-binding: t; -*-

;; This mode implements syntax highlighting for strings with interpolation (ala
;; Terraform configuration syntax):
;;
;;   "This is a string ${some-value-to-interpolate("with nesting!")}"
;;
;; You can escape interpolation with double dollar signs: "$${foo}" will be
;; rendered as ${foo}. Quotes can be escaped with a backslash : "\"" will be
;; rendered as ".
;;
;; Notably, we allow strings and string interpolation to span multiple lines.

(defconst INTERP-TOKEN-QUOTE 'quote)
(defconst INTERP-TOKEN-OPEN-INTERP 'open-interpolation)
(defconst INTERP-TOKEN-CLOSE-INTERP 'close-interpolation)

(defconst INTERP-SPECIAL-TOKEN-ALIST
  `(("\"" . ,INTERP-TOKEN-QUOTE)
    ("${" . ,INTERP-TOKEN-OPEN-INTERP)
    ("}"  . ,INTERP-TOKEN-CLOSE-INTERP)))

(defconst interp-special-token-regex "\"\\|\\(${\\)\\|\\($}\\)")

(regexp-opt ("\"" "${" "}"))

(defun interp-special-token-type (special-token-string))

(defun interp-token (type start end)
  (:type type :start start :end end))

;; Let's use recursion rather than loops
(defun interp-remaining-tokens ()
  (let ((token-end (re-search-forward special-token-regex nil t)))
    (if token-end
        (let* ((matched-string (match-string 0))
               (match-start (match-beginning 0))
               (special-token-type (cond ((equal matched-string "\"") INTERP-TOKEN-QUOTE)
                                        ((equal matched-string "${") INTERP-TOKEN-))))))))

(defun interp-lex()
  (goto-char (point-min))
  (let ((tokens ())
        (token-type 'TEXT)
        (token-start (point))
        token-end)
    (while (setq token-end (re-search-forward "\"\\|\\(${\\)\\|\\($}\\)" nil t))
      (when (< token-start (point))
        (setq (cons (interp-token token-type token-start token-end) tokens)))
      
      ))

(type-of :foo)
