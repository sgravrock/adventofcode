#lang racket

(define (parse-line line)
  (string-split
   (string-replace
    (string-replace line "," "")
    "<-> "
    "")
   " "))

(provide parse-line)