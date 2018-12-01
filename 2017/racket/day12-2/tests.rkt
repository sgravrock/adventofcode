#lang racket

(require rackunit "lib.rkt")


(check-equal? (parse-line "0 <-> 2") '("0" "2"))
(check-equal? (parse-line "2 <-> 0, 3, 4") '("2" "0" "3" "4"))


(let ([sample-input '(("0" "2")
                      ("1" "1")
                      ("2" "0" "3" "4")
                      ("3" "2" "4")
                      ("4" "2" "3" "6")
                      ("5" "6")
                      ("6" "4" "5")
                      ("7" "1")
                     )])

  ; Sort and check equality rather than using set=?,
  ; so that failure messages include the expected and actual values
  (check-equal? (sort (set-union-all '((1 2) (1 3) (1 4))) <) '(1 2 3 4))

  (check-equal? (map (lambda (s) (sort s  string<?)) (find-groups sample-input))
                '(("1" "7") ("0" "2" "3" "4" "5" "6")))
)
