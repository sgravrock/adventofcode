#lang racket

(require "aoc.rkt")

(define (safe? lst)
  (any? (lambda (candidate)
          (define pairs (each-pair candidate))
          (and (or (all-increasing? pairs) (all-decreasing? pairs))
               (all-close-enough? pairs)))
        (with-one-skipped lst)))

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

(define (with-one-skipped lst)
  (cond [(empty? lst) '()]
        [(empty? (cdr lst)) '()]
        [(empty? (cddr lst))
         (list (list (car lst)) (cdr lst))]
        [else
         (cons
          (cdr lst)
          (map (lambda (tail)
                 (cons (car lst) tail))
               (with-one-skipped (cdr lst))))]))

(define input (map (lambda (line)
                     (map string->number
                          (string-split line " ")))
                   (readlines)))
(length (filter safe? input))