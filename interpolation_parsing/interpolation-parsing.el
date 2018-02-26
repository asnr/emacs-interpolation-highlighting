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

(defconst INTERP-TOKEN-TEXT 'text)
(defconst INTERP-TOKEN-QUOTE 'quote)
(defconst INTERP-TOKEN-OPEN-INTERP 'open-interpolation)
(defconst INTERP-TOKEN-CLOSE-INTERP 'close-interpolation)

(defconst INTERP-SPECIAL-TOKEN-ALIST
  `(("\"" . ,INTERP-TOKEN-QUOTE)
    ("${" . ,INTERP-TOKEN-OPEN-INTERP)
    ("}"  . ,INTERP-TOKEN-CLOSE-INTERP)))

(defconst interp-special-token-regex (regexp-opt '("\"" "${" "}")))

(defun interp-special-token-type (special-token-string)
  (cdr (assoc special-token-string INTERP-SPECIAL-TOKEN-ALIST)))

(defun interp-token (type start end)
  `(:type ,type :start ,start :end ,end))

(defun interp-remaining-tokens ()
  (if (eobp)
      ()
    (let ((start-point (point))
          (special-token-end (re-search-forward interp-special-token-regex nil t)))
      (if special-token-end
          (let* ((matched-string (match-string 0))
                 (match-start (match-beginning 0))
                 (special-token-type (interp-special-token-type matched-string))
                 (special-token (interp-token special-token-type
                                              match-start
                                              special-token-end)))
            (if (equal start-point match-start)
                (cons special-token (interp-remaining-tokens))
              (cons (interp-token INTERP-TOKEN-TEXT start-point match-start)
                    (cons special-token (interp-remaining-tokens)))))
        (cons INTERP-TOKEN-TEXT start-point (point-max))))))

(defun interp-lex()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Lex result: %s"(interp-remaining-tokens))))
