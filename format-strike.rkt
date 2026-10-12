#lang racket

;; STRIKE-TEXT.
;;
;; This module allows us to format a strikethrough text. It relies on ANSI
;; escape codes (concretely, Select Graphic Rendition parameters) to format the
;; text. As a result, it could be problematic in older terminals or in DrRacket
;; and we keep it disabled by default.
;;
;; You may activate the strike-text mode by uncommenting the first line and
;; commenting the second.

;;(define (strike-ansi str) (format "\u001b[9m~a\u001b[29m" str))
(define (strike-ansi str) str)

(provide strike-ansi)
