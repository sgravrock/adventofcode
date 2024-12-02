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
                     (abs (- (car pair) (cadr pair))))
                   pairs))
(foldl + 0 diffs)