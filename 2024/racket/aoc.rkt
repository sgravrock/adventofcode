#lang racket

(provide readlines read-input regexp-captures regexp-match->captures count-if
         each-pair all? any? first-by
         coord coord-x coord-y coord-+x coord-+y parse-grid)

(define (read-input line-re captures->rec [port (current-input-port)])
  (map (lambda (line)
         (captures->rec (regexp-captures line-re line)))
       (readlines port)))

(define (regexp-captures needle haystack)
  (define m (regexp-match-positions needle haystack))
  (cond [m (regexp-match->captures (cdr m) haystack)]
        [else #f]))

(define (regexp-match->captures match input)
  (sequence->list (map (lambda(capture)
                         (substring input (car capture) (cdr capture)))
                       match)))

(define (readlines [port (current-input-port)])
  (define lines (string-split (port->string port) "\n"))
  (filter non-empty-string? lines)) ; esp. to take care of trailing blank lines

(struct coord (x y) #:inspector #f)

(define (coord-+x c dx)
  (coord (+ (coord-x c) dx) (coord-y c)))

(define (coord-+y c dy)
  (coord (coord-x c) (+ (coord-y c) dy)))

(define (parse-grid)
  (define grid (hash))
  (for ([(line y) (in-indexed (readlines))])
    (for ([(c x) (in-indexed line)])
      (set! grid (hash-set grid (coord x y) c))))
  grid)

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

(define (first-by keyfunc lst)
  (define decorated (map (lambda (x) (cons x (keyfunc x))) lst))
  (car (first-by-cdr decorated)))

(define (first-by-cdr lst)
  (cond [(empty? lst) #f]
        [(empty? (cdr lst)) (car lst)]
        [else
         (define entry (car lst))
         (define tail-result (first-by-cdr (cdr lst)))
         (cond [(< (cdr entry) (cdr tail-result)) entry]
               [else tail-result])]))