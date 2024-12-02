#lang racket

(require "aoc.rkt")

(define (safe? lst)
  (define pairs (each-pair lst))
  (and (or (all-increasing? pairs) (all-decreasing? pairs))
       (all-close-enough? pairs)))

(define (all-increasing? pairs)
  (all? (lambda (pair)
          (< (car pair) (cdr pair)))
        pairs))

(define (all-decreasing? pairs)
  (all? (lambda (pair)
          (> (car pair) (cdr pair)))
        pairs))

(define (all-close-enough? pairs)
  (all? (lambda (pair)
          (define delta (abs (- (car pair) (cdr pair))))
          (and (>= delta 1) (<= delta 3)))
        pairs))


(define input (map (lambda (line)
                     (map string->number
                          (string-split line " ")))
                   (readlines)))
(length (filter safe? input))
