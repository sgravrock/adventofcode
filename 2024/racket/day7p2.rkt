#lang racket

(require "aoc.rkt")

(struct equation (result operands) #:inspector #f)

(define (parse-input)
  (map (lambda (line)
         (define chunks (string-split line ": "))
         (equation (string->number (car chunks))
                   (map string->number (string-split (cadr chunks) " "))))
       (readlines)))

(define (solve eqns)
  (define possibles (filter possible? eqns))
  (foldl + 0 (map equation-result possibles)))

(define (possible? eqn)
  (possible-with-ors? (equation-result eqn)
                      (reverse (equation-operands eqn))))

(define (possible-with-ors? expected reversed-operands)
  (cond [(empty? reversed-operands) #f]
        [(empty? (cdr reversed-operands))
         (equal? expected (car reversed-operands))]
        [else (or (possible-with-ors? (- expected (car reversed-operands))
                                      (cdr reversed-operands))
                  (possible-with-ors? (/ expected (car reversed-operands))
                                      (cdr reversed-operands))
                  (possible-with-initial-or? expected reversed-operands))]))

; Precondition: reversed-operands has at least two elements
(define (possible-with-initial-or? expected reversed-operands)
  (cond [(not (integer? expected)) #f] ; Can't make a fraction by concatenation
        [else
         (define sExpected (number->string expected))
         (define suffix (number->string (car reversed-operands)))
         (cond [(not (string-suffix? sExpected suffix)) #f]
               [else
                (define prefix (substring sExpected
                                          0
                                          (- (string-length sExpected) (string-length suffix))))
                (possible-with-ors? (string->number prefix)
                                    (cdr reversed-operands))])]))

(define eqns (parse-input))
(solve eqns)