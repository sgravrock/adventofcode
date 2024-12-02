#lang racket

(provide readlines read-input count-if each-pair all? any?)

(define (read-input line-re captures->rec [port (current-input-port)])
  (map (lambda (line)
         (captures->rec (regexp-captures line-re line)))
       (readlines port)))

(define (regexp-captures needle haystack)
  (define m (regexp-match-positions needle haystack))
  (cond [m (sequence->list (map (lambda(capture)
                                  (substring haystack (car capture) (cdr capture)))
                                (cdr m)))]
        [else #f]))

(define (readlines [port (current-input-port)])
  (define lines (string-split (port->string port) "\n"))
  (filter non-empty-string? lines)) ; esp. to take care of trailing blank lines

(define (count-if pred seq)
  (foldl (lambda (el acc)
           (if (pred el)
               (+ acc 1)
               acc))
         0
         seq))

(define (each-pair lst)
  (cond [(empty? lst) '()]
        [else (each-pair-sub (car lst) (cdr lst))]))

(define (each-pair-sub prev lst)
  (cond [(empty? lst) '()]
        [else (cons (cons prev (car lst)) (each-pair-sub (car lst) (cdr lst)))]))

(define (all? pred lst)
  (cond [(empty? lst) #t] ; vacuously true
        [(not (pred (car lst))) #f]
        [else (all? pred (cdr lst))]))

(define (any? pred lst)
  (cond [(empty? lst) #f]
        [(pred (car lst)) #t]
        [else (any? pred (cdr lst))]))