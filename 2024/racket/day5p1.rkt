#lang racket

(require "aoc.rkt")

(define (parse-input)
  (define input (port->string (current-input-port)))
  (define top-level-chunks (map (lambda (chunk)
                                  (string-split chunk "\n"))
                                (string-split input "\n\n")))
  (define ordering-rules (map (lambda (line)
                                (map string->number (string-split line "|")))
                              (car top-level-chunks)))
  (define updates (map (lambda (line)
                         (map string->number (string-split line ",")))
                       (cadr top-level-chunks)))
  (values ordering-rules updates))

(define (solve ordering-rules updates)
  (define valid-updates (filter (lambda (u) (valid? u ordering-rules)) updates))
  (foldl + 0 (map middle valid-updates)))

(define (middle lst)
  (define vec (list->vector lst))
  (vector-ref vec (quotient (vector-length vec) 2)))

(define (valid? update ordering-rules)
  (define conflicting-rules
    (filter (lambda (rule)
              (precedes? (cadr rule) (car rule) update)
              )
            ordering-rules))
  (empty? conflicting-rules))

(define (precedes? a b lst)
  (cond [(empty? lst) #f]
        [(eq? (car lst) a) (member b (cdr lst))]
        [else (precedes? a b (cdr lst))]))

(define-values (ordering-rules updates) (parse-input))
(solve ordering-rules updates)