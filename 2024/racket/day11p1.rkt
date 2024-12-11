#lang racket

(require "aoc.rkt")

(define (parse-input)
  (define line (car (readlines)))
  (map string->number (string-split line " ")))

(define (solve initial-stones)
  (define stones initial-stones)
  (for ([i 25])
    (set! stones (blink stones)))
  (length stones))

(define (blink lst)
  (cond [(empty? lst) '()]
        [else
         (define rest (blink (cdr lst)))
         (cond [(equal? 0 (car lst))
                (cons 1 rest)]
               [(even-digits? (car lst))
                (define new-stones (split (car lst)))
                (cons (car new-stones) (cons (cdr new-stones) rest))]
               [else
                (cons (* 2024 (car lst)) rest)])]))

(define (even-digits? n)
  (= 0 (remainder (string-length (number->string n)) 2)))

(define (split stone)
  (define s (number->string stone))
  (define break (/ (string-length s) 2))
  (cons (string->number (substring s 0 break))
        (string->number (substring s break))))

(define stones (parse-input))
(solve stones)