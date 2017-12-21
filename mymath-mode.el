(defvar mymath-mode-syntax-table nil "Syntax table for `mymath-mode'.")

;; Syntax tables won't be effective, because ‘arst'
(setq mymath-mode-syntax-table
      (let ((syntax-table (make-syntax-table)))
        ;; Next 2 lines don't work, syntax tables only have one entry per char
        ;; (modify-syntax-entry ?\! "<" syntax-table)
        ;; (modify-syntax-entry ?\! ">" syntax-table)
        (modify-syntax-entry ?\! "\"" syntax-table)
        (modify-syntax-entry ?\? "\!" syntax-table)
        ;; The next 4 lines appear in lisp-mode.el. I thought they were used for
        ;; highlighting comments and the `symbols' inside comments, but the symbols
        ;; inside of comments is done using font lock keywords, which is why we
        ;; can use weird quote-tick combos: ‘just' and `wat’
        ;; (modify-syntax-entry ?\n ">   " syntax-table)
        ;; (modify-syntax-entry ?\; "<   " syntax-table)
        ;; (modify-syntax-entry ?` "'   " syntax-table)
        ;; (modify-syntax-entry ?' "'   " syntax-table)
        syntax-table))

;; font-lock-extend-region-functions  <-  add function here
;; font-lock-extend-region-function   <-  hook that function also needs to go into
;; Somehow identify multiline construct when inserted into buffer, and before
;; font lock tries to highlight it, mark with `font-lock-multiline'

;; For rehighlighting, let's depend on setting `jit-lock-contextually' to t

(defun my-end-of-paragraph-position (&rest foo)
  "Return position of next empty line."
  (save-excursion
    (while (not (or (eobp)                ; Stop at end of buffer.
                    (and (bolp) (eolp)))) ; Or at an empty line.
      (forward-line))
    (point)))

(defun my-start-of-paragraph-position ()
  (save-excursion
    (while (not (or (bobp)
                    (and (bolp) (eolp))))
      (forward-line -1))
    (point)))

(defun extend-region-to-paragraph ()
  (let* ((paragraph-start (my-start-of-paragraph-position))
         (paragraph-end (my-end-of-paragraph-position))
         (adjust-beg (< paragraph-start font-lock-beg))
         (adjust-end (> paragraph-end font-lock-end)))
    (when adjust-beg (setq font-lock-beg paragraph-start))
    (when adjust-end (setq font-lock-end paragraph-end))
    (or adjust-beg adjust-end)))

(setq mymath-highlights
      '(("Sin\\|Cos\\|Sum" . font-lock-function-name-face)
        ("Pi\\|Infinity" . font-lock-constant-face)
        (":.*:" . font-lock-string-face)
        ("^FOO"
         (0 font-lock-keyword-face)  ;; subexp-highlighter - face for FOO
         (".*\\(\n.+\\)*BAR" (my-end-of-paragraph-position) nil (0 font-lock-variable-name-face)))))

(define-derived-mode mymath-mode fundamental-mode "mymath"
  "major mode for editing mymath language code."
  (set-syntax-table mymath-mode-syntax-table)
  (setq font-lock-defaults '(mymath-highlights))
  ;; (setq font-lock-multiline t)  Is this on by default?
  (add-hook 'font-lock-extend-region-functions 'extend-region-to-paragraph)
  )
