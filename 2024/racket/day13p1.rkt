#lang racket

(require "aoc.rkt")
(require threading)
(require mischief/stream)

(struct machine (targetX targetY aXRate aYRate bXRate bYRate) #:inspector #f)

(define (parse-input [lines (readlines)])
  (cond [(empty? lines) '()]
        [else (define-values (m remaining-lines) (parse-machine lines))
              (cons m (parse-input remaining-lines))]))

(define (parse-machine lines)
  (define button-re #rx"^Button [AB]: X\\+([0-9]+), Y\\+([0-9]+)$")
  (define prize-re #rx"^Prize: X=([0-9]+), Y=([0-9]+)$")
  (define a-line (car lines))
  (define b-line (car (cdr lines)))
  (define prize-line (car (cdr (cdr lines))))
  (define rest (cdr (cdr (cdr lines)))); also consume blank line
  (define a-nums (map string->number (regexp-captures button-re a-line)))
  (define b-nums (map string->number (regexp-captures button-re b-line)))
  (define prize-nums (map string->number (regexp-captures prize-re prize-line)))
  (values (machine (car prize-nums) (cadr prize-nums)
                   (car a-nums) (cadr a-nums)
                   (car b-nums) (cadr b-nums))
          rest))

(define (solve machines)
  (~> machines
      (map (lambda (m) (score-for-machine m))
           _)
      (foldl + 0 _)))

(define (score-for-machine m)
  (define solns (solutions-for-machine m))
  (cond [(empty? solns) 0]
        [(empty? (cdr solns))
         (define a (car (car solns)))
         (define b (cdr (car solns)))
         (+ (* 3 a) b)]
        [else
         ; Valid, but hasn't happened yet
         (raise "multiple solutions")]))
   

(define (solutions-for-machine m)
  (~> (stream-cross-product (lambda (a b) (cons a b))
                            (in-range 101)
                            (in-range 101))
      (sequence-filter (lambda (ab)
                         (and (equal? (machine-targetX m)
                                      (+ (* (machine-aXRate m) (car ab))
                                         (* (machine-bXRate m) (cdr ab))))
                              (equal? (machine-targetY m)
                                      (+ (* (machine-aYRate m) (car ab))
                                         (* (machine-bYRate m) (cdr ab))))))
                       _)
      (sequence->list)))

(define machines (parse-input))
(solve machines)