#lang racket

(require seq)
(require "aoc.rkt")

(define input (read-input #rx"^([0-9]+)   ([0-9]+)$"
                          (lambda (captures)
                            (map string->number captures))))
(define lists (unzip input))
(define sorted-lists (sequence->list (map (lambda (lst)
                                            (sort lst <))
                                          lists)))
(define pairs (zip (car sorted-lists) (cadr sorted-lists)))
(define diffs (map (lambda (pair)
                     (define a (car pair))
                     (define b (cadr pair))
                     (cond [(< a b) (- b a)]
                           [else (- a b)]))
                   pairs))
(foldl + 0 diffs)