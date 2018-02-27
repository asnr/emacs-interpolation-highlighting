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

(defconst INTERP-NODE-TOP 'top)
(defconst INTERP-NODE-TOP-EMPTY 'top-empty)
(defconst INTERP-NODE-STRING 'string)
(defconst INTERP-NODE-STRING-CONTENTS 'string-contents)
(defconst INTERP-NODE-STRING-CONTENTS-EMPTY 'string-contents-empty)

(defun interp-special-token-type (special-token-string)
  (cdr (assoc special-token-string INTERP-SPECIAL-TOKEN-ALIST)))

(defun interp-token (type start end)
  `(:type ,type :start ,start :end ,end))

(defun interp-token-type (token)
  (plist-get token :type))

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
        (cons (interp-token INTERP-TOKEN-TEXT start-point (point-max)) ())))))

(defun interp-lex ()
  (save-excursion
    (goto-char (point-min))
    (interp-remaining-tokens)))

(defun interp-node-top (text-or-string top)
  `(:type ,INTERP-NODE-TOP :head ,text-or-string :tail ,top))

(defun interp-node-top-empty ()
  `(:type ,INTERP-NODE-TOP-EMPTY))

(defun interp-node-string (open-quote contents close-quote)
  `(:type ,INTERP-NODE-STRING
    :open-quote ,open-quote
    :contents ,contents
    :close-quote ,close-quote))

(defun interp-node-string-contents (head string-contents)
  `(:type ,INTERP-NODE-STRING-CONTENTS :head ,head :tail ,string-contents))

(defun interp-node-string-contents-empty ()
  `(:type ,INTERP-NODE-STRING-CONTENTS-EMPTY))

(defun parse-result (node remaining-tokens)
  (cons node remaining-tokens))

(defun interp-parse ()
  (interactive)
  (let ((tokens (interp-lex)))
    (message "--------------------------")
    (message "Tokens: %s" tokens)
    (message "Parse:")
    (pp (interp-parse-top tokens))))

(defun interp-parse-top (tokens)
  (if tokens
      (let* ((next-token (car tokens))
             (next-token-type (interp-token-type next-token)))
        (cond ((equal INTERP-TOKEN-TEXT next-token-type)
               (interp-node-top next-token (interp-parse-top (cdr tokens))))
              ((equal INTERP-TOKEN-QUOTE next-token-type)
               (let* ((parse-result (interp-parse-string tokens))
                      (string-node (car parse-result))
                      (remaining-tokens (cdr parse-result)))
                 (interp-node-top string-node (interp-parse-top remaining-tokens))))
              (t
               (interp-node-top next-token (interp-parse-top (cdr tokens))))))
    (interp-node-top-empty)))

(defun interp-parse-string (tokens)
  (let* ((open-quote (car tokens))
         (contents-parse-result (interp-parse-string-contents (cdr tokens)))
         (string-contents-node (car contents-parse-result))
         (remaining-tokens (cdr contents-parse-result))
         (close-quote (car remaining-tokens))
         (tokens-after-string (cdr remaining-tokens)))
    (parse-result (interp-node-string open-quote string-contents-node close-quote)
                  tokens-after-string)))

(defun interp-parse-string-contents (tokens)
  (if tokens
      (let* ((this-token (car tokens))
             (next-tokens (cdr tokens))
             (this-token-type (interp-token-type this-token)))
        (cond ((equal INTERP-TOKEN-QUOTE this-token-type)
               (parse-result (interp-node-string-contents-empty) tokens))
              (t
               (let* ((parse-result (interp-parse-string-contents next-tokens))
                      (remaining-contents-node (car parse-result))
                      (tokens-after-parse (cdr parse-result)))
                 (parse-result (interp-node-string-contents this-token
                                                            remaining-contents-node)
                               tokens-after-parse)))))
    (parse-result (interp-node-string-contents-empty) tokens)))
