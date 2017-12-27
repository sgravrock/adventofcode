#lang racket

(require rackunit "lib.rkt")


(check-equal? (parse-line "0 <-> 2") '("0" "2"))
(check-equal? (parse-line "2 <-> 0, 3, 4") '("2" "0" "3" "4"))
