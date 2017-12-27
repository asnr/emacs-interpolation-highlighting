;; -*- lexical-binding: t; -*-

;; This mode implements syntax highlighting for strings with interpolation (ala
;; Terraform configuration syntax):
;;
;;   "This is a string ${some-value-to-interpolate}"
;;
;; Notably, we allow strings to span multiple lines.

;; We will let the syntax table take care of highlighting strings and will
;; highlight the interpolated regions with `font-lock-defaults' and
;; `font-lock-extend-region-functions'.

(defconst interpolation-regex "\\${\\([^}]\\|\n\\)*}?")

(setq interpolation-highlights `((,interpolation-regex . font-lock-constant-face)))

(define-derived-mode interpolation-mode fundamental-mode "interpolation"
  (setq font-lock-defaults '(interpolation-highlights))
  (setq font-lock-multiline t))
