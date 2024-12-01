#lang racket

(require seq)
(require "aoc.rkt")

(define (similarity-score n right-list)
  (define count (length (filter (lambda (x) (= n x)) right-list)))
  (* n count))

(define input (read-input #rx"^([0-9]+)   ([0-9]+)$"
                          (lambda (captures)
                            (map string->number captures))))
(define lists (sequence->list (unzip input)))
(define left (car lists))
(define right (cadr lists))
(foldl + 0 (map (lambda (x) (similarity-score x right)) left))