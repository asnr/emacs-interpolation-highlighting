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
(defconst INTERP-NODE-INTERPOLATION 'interpolation)
(defconst INTERP-NODE-INTERPOLATION-CONTENTS 'interpolation-contents)
(defconst INTERP-NODE-INTERPOLATION-CONTENTS-EMPTY 'interpolation-contents-empty)

(defun interp-special-token-type (special-token-string)
  (cdr (assoc special-token-string INTERP-SPECIAL-TOKEN-ALIST)))

(defun interp-token-highlight (token &optional highlight-face)
  (when highlight-face
    (let ((start-point (plist-get token :start))
          (end-point (plist-get token :end)))
      (add-text-properties start-point end-point `(face ,highlight-face)))))

(defun interp-token (type start end)
  `(:type ,type :start ,start :end ,end :highlight interp-token-highlight))

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
  `(:type ,INTERP-NODE-TOP
    :head ,text-or-string
    :tail ,top
    :highlight interp-list-node-highlight))

(defun interp-list-node-highlight (node &optional highlight-face)
  (let* ((head-node (plist-get node :head))
         (head-highlight (plist-get head-node :highlight))
         (tail-node (plist-get node :tail))
         (tail-highlight (plist-get tail-node :highlight)))
    (apply head-highlight (list head-node highlight-face))
    (apply tail-highlight (list tail-node highlight-face))))

(defun interp-node-top-empty ()
  `(:type ,INTERP-NODE-TOP-EMPTY :highlight interp-node-highlight-do-nothing))

(defun interp-node-highlight-do-nothing (&rest args)
  nil)

(defun interp-node-string (open-quote contents close-quote)
  `(:type ,INTERP-NODE-STRING
    :open-quote ,open-quote
    :contents ,contents
    :close-quote ,close-quote
    :highlight interp-node-string-highlight))

(defun interp-node-string-highlight (string-node &optional highlight-face)
  (let* ((open-quote-node (plist-get string-node :open-quote))
          (open-quote-highlight (plist-get open-quote-node :highlight))
          (contents-node (plist-get string-node :contents))
          (contents-highlight (plist-get contents-node :highlight))
          (close-quote-node (plist-get string-node :close-quote))
          (close-quote-highlight (plist-get close-quote-node :highlight)))
     (apply open-quote-highlight `(,open-quote-node font-lock-string-face))
     (apply contents-highlight `(,contents-node font-lock-string-face))
     (apply close-quote-highlight `(,close-quote-node font-lock-string-face))))

(defun interp-node-string-contents (head string-contents)
  `(:type ,INTERP-NODE-STRING-CONTENTS
    :head ,head
    :tail ,string-contents
    :highlight interp-list-node-highlight))

(defun interp-node-string-contents-empty ()
  `(:type ,INTERP-NODE-STRING-CONTENTS-EMPTY
    :highlight interp-node-highlight-do-nothing))

(defun interp-node-interpolation (open-brace contents close-brace)
  `(:type ,INTERP-NODE-INTERPOLATION
    :open-brace ,open-brace
    :contents ,contents
    :close-brace ,close-brace
    :highlight interp-node-interpolation-highlight))

(defun interp-node-interpolation-highlight (interpolation-node &optional face-ignored)
  (let* ((open-brace-node (plist-get interpolation-node :open-brace))
         (open-brace-highlight (plist-get open-brace-node :highlight))
         (contents-node (plist-get interpolation-node :contents))
         (contents-highlight (plist-get contents-node :highlight))
         (close-brace-node (plist-get interpolation-node :close-brace))
         (close-brace-highlight (plist-get close-brace-node :highlight)))
    (apply open-brace-highlight `(,open-brace-node font-lock-variable-name-face))
    (apply contents-highlight `(,contents-node font-lock-variable-name-face))
    (apply close-brace-highlight `(,close-brace-node font-lock-variable-name-face))))

(defun interp-node-interpolation-contents (head interpolation-contents)
  `(:type ,INTERP-NODE-INTERPOLATION-CONTENTS
    :head ,head
    :tail ,interpolation-contents
    :highlight interp-list-node-highlight))

(defun interp-node-interpolation-contents-empty ()
  `(:type ,INTERP-NODE-INTERPOLATION-CONTENTS-EMPTY
    :highlight interp-node-highlight-do-nothing))

(defun parse-result (node remaining-tokens)
  (cons node remaining-tokens))

(defun interp-parse ()
  (interactive)
  (let ((tokens (interp-lex)))
    (interp-parse-top tokens)))

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
              ((equal INTERP-TOKEN-OPEN-INTERP this-token-type)
               (let* ((interpolation-parse (interp-parse-interpolation tokens))
                      (interpolation-node (car interpolation-parse))
                      (tokens-after-interpolation (cdr interpolation-parse))
                      (remaining-contents-parse
                       (interp-parse-string-contents tokens-after-interpolation))
                      (remaining-contents-node (car remaining-contents-parse))
                      (tokens-after-all-contents (cdr remaining-contents-parse)))
                 (parse-result (interp-node-string-contents interpolation-node
                                                            remaining-contents-node)
                               tokens-after-all-contents)))
              (t
               (let* ((parse-result (interp-parse-string-contents next-tokens))
                      (remaining-contents-node (car parse-result))
                      (tokens-after-parse (cdr parse-result)))
                 (parse-result (interp-node-string-contents this-token
                                                            remaining-contents-node)
                               tokens-after-parse)))))
    (parse-result (interp-node-string-contents-empty) tokens)))

(defun interp-parse-interpolation (tokens)
  (let* ((open-brace (car tokens))
         (contents-parse-result (interp-parse-interpolation-contents (cdr tokens)))
         (interpolation-contents-node (car contents-parse-result))
         (remaining-tokens (cdr contents-parse-result))
         (close-brace (car remaining-tokens))
         (tokens-after-interpolation (cdr remaining-tokens)))
    (parse-result (interp-node-interpolation open-brace
                                             interpolation-contents-node
                                             close-brace)
                  tokens-after-interpolation)))

(defun interp-parse-interpolation-contents (tokens)
  (if tokens
      (let* ((this-token (car tokens))
             (next-tokens (cdr tokens))
             (this-token-type (interp-token-type this-token)))
        (cond ((equal INTERP-TOKEN-CLOSE-INTERP this-token-type)
               (parse-result (interp-node-interpolation-contents-empty) tokens))
              ((equal INTERP-TOKEN-QUOTE this-token-type)
               (let* ((string-parse (interp-parse-string tokens))
                      (string-node (car string-parse))
                      (tokens-after-string (cdr string-parse))
                      (remaining-contents-parse
                       (interp-parse-interpolation-contents tokens-after-string))
                      (remaining-contents-node (car remaining-contents-parse))
                      (tokens-after-all-contents (cdr remaining-contents-parse)))
                 (parse-result
                  (interp-node-interpolation-contents string-node
                                                      remaining-contents-node)
                  tokens-after-all-contents)))
              (t
               (let* ((parse-result (interp-parse-interpolation-contents next-tokens))
                      (remaining-contents-node (car parse-result))
                      (tokens-after-parse (cdr parse-result)))
                 (parse-result
                  (interp-node-interpolation-contents this-token
                                                      remaining-contents-node)
                  tokens-after-parse)))))
    (parse-result (interp-node-interpolation-contents-empty) tokens)))

(defun interp-highlight ()
  (interactive)
  (let ((top-node-of-parse-tree (interp-parse)))
    (interp-list-node-highlight top-node-of-parse-tree)))
