#lang racket

(require "aoc.rkt")
(require threading)

(define (parse-input)
  (define line (car (readlines)))
  (map string->number (string-split line " ")))

(define (solve stones)
  (define memo (make-hash))
  (~> stones
      (map (lambda (stone)
             (count-after-blinking stone 75 memo))
           _)
      (foldl + 0 _)))


(define (count-after-blinking stone times memo)
  (define key (cons stone times))
  (define memoized (hash-ref memo key #f))
  (cond [(number? memoized) memoized]
        [else
         (define blinked (blink-once stone))
         (define done (equal? times 1))
         (define result (cond [(and done (cons? blinked)) 2]
                              [done 1]
                              [(cons? blinked)
                               (+ (count-after-blinking (car blinked) (- times 1) memo)
                                  (count-after-blinking (cdr blinked) (- times 1) memo))]
                              [else (count-after-blinking blinked (- times 1) memo)]))
         (hash-set! memo key result)
         result]))

(define (blink-once stone)
  (cond [(equal? 0 stone) 1]
        [(even-digits? stone) (split stone)]
        [else (* 2024 stone)]))
  

(define (even-digits? n)
  (= 0 (remainder (string-length (number->string n)) 2)))

(define (split stone)
  (define s (number->string stone))
  (define break (/ (string-length s) 2))
  (cons (string->number (substring s 0 break))
        (string->number (substring s break))))

(define stones (parse-input))
(solve stones)