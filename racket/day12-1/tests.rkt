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

  (let ([called #f])
    (check-false (debug-if #f (lambda () (set! called #t))))
    (check-false called))
  (let ([called #f])
    (check-true (debug-if #t (lambda () (set! called #t))))
    (check-true called))
  
  (check-equal? (find-line "5" sample-input) '("5" "6"))
  
  (check-true (reaches-directly '("4" "2") "4"))
  (check-false (reaches-directly '("4" "2") "3"))
  
  (check-false (reaches-via '("1" "1") "1" "0" sample-input))
  (check-true  (reaches-via '("0" "1") "1" "0" sample-input))
  (check-true (reaches-via '("2" "0" "3" "4") "4" "0" sample-input))
  (check-false (reaches-via '("2" "0" "3" "4") "5" "0" sample-input))
  (check-false (reaches-via '("7" "1") "1" "0" sample-input))

  (check-true (reaches "0" "0" sample-input))
  (check-true (reaches "2" "0" sample-input))
  (check-false (reaches "1" "0" sample-input))

  (check-equal? (all-that-reach "0" sample-input) '("0" "2" "3" "4" "5" "6"))
)
