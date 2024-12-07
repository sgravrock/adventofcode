#lang racket

(require "aoc.rkt")

(struct equation (result operands) #:inspector #f)

(define (parse-input)
  (map (lambda (line)
         (define chunks (string-split line ": "))
         (equation (string->number (car chunks))
                   (map string->number (string-split (cadr chunks) " "))))
       (readlines)))

(define (possible? eqn)
  (define (possible-sub? expected operands)
    (cond [(empty? (cdr operands))
           (eq? expected (car operands))]
          [else (or (possible-sub?  (- expected (car operands)) (cdr operands))
                    (possible-sub? (/ expected (car operands)) (cdr operands)))]))
  (possible-sub? (equation-result eqn) (reverse (equation-operands eqn))))

(define equations (parse-input))
(foldl + 0 (map equation-result (filter possible? equations)))