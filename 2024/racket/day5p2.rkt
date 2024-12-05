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
  (define invalid-updates (filter (lambda (u)
                                    (not (valid? u ordering-rules)))
                                  updates))
  (define fixed (map (lambda (u)
                       (fix u ordering-rules))
                     invalid-updates))
  (foldl + 0 (map middle fixed)))


(define (middle lst)
  (define vec (list->vector lst))
  (vector-ref vec (quotient (vector-length vec) 2)))


(define (invalid-updates updates ordering-rules)
  (filter (lambda (u) (first-conflicting-rule u ordering-rules))
          updates))


(define (valid? update ordering-rules)
  (cond [(eq? #f (first-conflicting-rule update ordering-rules))
         #t]
        [else
         #f]))

(define (first-conflicting-rule update ordering-rules)
  (cond [(empty? ordering-rules) #f]
        [else (define rule (car ordering-rules))
              (cond [(precedes? (cadr rule) (car rule) update) rule]
                    [else (first-conflicting-rule update (cdr ordering-rules))])]))

(define (fix update ordering-rules)
  (define conflict (first-conflicting-rule update ordering-rules))
  (cond [(false? conflict) update] ; already ok
        [else (fix (fix-one-rule update conflict) ordering-rules)]))

(define (fix-one-rule update rule)
  (define earlier-page (car rule))
  (define later-page (cadr rule))
  (insert-before earlier-page
                 later-page
                 (remove earlier-page update)))

(define (remove v lst)
  (filter (lambda (x) (not (eq? x v)))
          lst))

(define (insert-before el-to-insert sentinel lst)
  (cond [(empty? lst)
         (list el-to-insert)]
        [(eq? (car lst) sentinel)
         (cons el-to-insert (cons sentinel (cdr lst)))]
        [else
         (cons (car lst) (insert-before el-to-insert sentinel (cdr lst)))]))

(define (precedes? a b lst)
  (cond [(empty? lst) #f]
        [(eq? (car lst) a) (member b (cdr lst))]
        [else (precedes? a b (cdr lst))]))

(define-values (ordering-rules updates) (parse-input))
(solve ordering-rules updates)
