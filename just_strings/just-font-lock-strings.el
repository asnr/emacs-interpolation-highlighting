;; -*- lexical-binding: t; -*-

;; This file implements string highlighting just with font-lock-defaults and
;; font-lock-extend-region-functions. (We allow strings to span multiple lines.)
;; We don't want to reread the buffer from the start to correctly rehighlight,
;; so we keep track of current string delimiters with the `string-delimiters'
;; buffer-local list.

;; We use `:' as the string delimiter, so that we don't have to deal with
;; disabling the string definition in the syntax table that we inherit from
;; `fundamental-mode'

;; Like many regexes, this is surprisingly fiddly.
;; - Naive greedy matching of the string contents (i.e. something like
;; `.*') wouldn't work because we would end up with one big string instead of
;; lots of small ones.
;; - The closing delimiter should be optional because an unmatched string
;; delimiter should match the rest of the document, thus alerting the user to
;; the dangling delimiter. (Maybe we don't need this feature?)
;; - Once we make the closing delimiter optional, lazy matching the string
;; contents will always result in the entire string being just the opening
;; delimiter `:', so we need to simulate lazy matching using `[^:]'
(defconst custom-string-regex ":\\([^:]\\|\n\\)*:?" )

(setq just-strings-highlights
      ;; Don't use string font face to make it easier to spot
      `((,custom-string-regex . font-lock-constant-face)))

(defun delimiters-in-region (start end)
  ;; We store the point immediately after the `:' in the delimiter list.
  ;; Note we're not attempting to deal with comments here. Doing so will add a
  ;; decent amount of complexity
  (let ((string-delimiters ()))
    (save-excursion
      (goto-char start)
      (while (and (search-forward ":" nil t) (<= (point) end))
        (setq string-delimiters (cons (point) string-delimiters)))
      (reverse string-delimiters))))

(defun update-string-delimiters (change-region-begin
                                 change-region-end
                                 original-region-length)
  (setq string-delimiters (delimiters-update string-delimiters
                                             change-region-begin
                                             change-region-end
                                             original-region-length)))

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

;; Extend the start of region to be syntax-highlighted to include the opening
;; delimiter if the start of the region falls inside of a delimiter pair.
;;
;; Why isn't the end of the region being extended as well? Good question.
;; Suppose the user enters a delimiter in the middle of an existing delimiter
;; pair, for example
;;
;;   outside :inside |the string: outside again    <-- '|' indicates cursor
;;
;; then
;;
;;   outside :inside :|the string: outside again
;;
;; This will cause all the following delimiter pairs to 'flip': everything that
;; was previously inside a string will be outside and vice versa. Thus if we
;; wanted to expand the region to capture all of the syntax highlighting
;; changes, we would have to extend it to the end of the document. This could
;; lead to a significant pause after inserting the character, which we want to
;; avoid. Thus we settle for occasionally incorrect, but always fast
;; re-highlighting and do not extend the end of the region out.
;;
;; It so happens that a few moments after the user stops entering text, while
;; emacs is idle, the whole document get re-highlighted. Thus even though our
;; re-highlighting may be wrong initially, this gets corrected soon enough. This
;; is the behaviour of string highlighting by the syntax table in
;; fundamental-mode as well, so at the very least we are delivering a
;; user-experience consistent with the rest of emacs.
(defun extend-region-to-string ()
  (let* ((start-of-delimiter-pair (start-of-delimiter-pair string-delimiters
                                                           font-lock-beg))
         (adjust-beg (< start-of-delimiter-pair font-lock-beg)))
    (when adjust-beg (setq font-lock-beg start-of-delimiter-pair))
    adjust-beg))


(defun start-of-delimiter-pair (delimiters position)
  (let* ((before-position-p (lambda (delimiter) (<= delimiter position)))
         (delimiters-before-position (seq-take-while before-position-p delimiters))
         (num-delimiters-before-position (seq-length delimiters-before-position))
         (position-outside-pair (zerop (mod num-delimiters-before-position 2))))
    (if position-outside-pair
        position
      (let ((opening-delimiter (elt (seq-reverse delimiters-before-position) 0)))
        (1- opening-delimiter)))))

(define-derived-mode just-strings-mode fundamental-mode "just-strings"
  (setq-local string-delimiters (delimiters-in-region (point-min) (point-max)))
  (setq font-lock-defaults '(just-strings-highlights))
  (setq font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions 'extend-region-to-string nil t)
  (add-hook 'after-change-functions 'update-string-delimiters nil t))
