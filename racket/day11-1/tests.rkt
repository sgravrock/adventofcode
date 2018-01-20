#lang racket

(require rackunit "lib.rkt")

(check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))

(check-equal? (neighbor '(0 0 0) "n") '(0 1 -1))
(check-equal? (neighbor '(0 0 0) "ne") '(1 0 -1))
(check-equal? (neighbor '(0 0 0) "se") '(1 -1 0))
(check-equal? (neighbor '(0 0 0) "s") '(0 -1 1))
(check-equal? (neighbor '(0 0 0) "sw") '(-1 0 1))
(check-equal? (neighbor '(0 0 0) "nw") '(-1 1 0))

(check-equal? (neighbor '(1 2 3) "n") '(1 3 2))
(check-equal? (neighbor '(1 2 3) "ne") '(2 2 2))
(check-equal? (neighbor '(1 2 3) "se") '(2 1 3))
(check-equal? (neighbor '(1 2 3) "s") '(1 1 4))
(check-equal? (neighbor '(1 2 3) "sw") '(0 2 4))
(check-equal? (neighbor '(1 2 3) "nw") '(0 3 3))