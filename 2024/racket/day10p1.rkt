#lang racket

(require "aoc.rkt")

(struct coord (x y) #:inspector #f)

(define (parse-input)
  (define result (make-hash))
  (define y 0)
  (for ([line (readlines)])
    (for ([x (string-length line)])
      (define s (substring line x (+ x 1)))
      (cond [(not (equal? s "."))
             (hash-set! result (coord x y) (string->number s))]))
    (set! y (+ y 1)))
  result)

(define (score grid)
  (define trailheads (filter (lambda (c)
                               (equal? 0 (hash-ref grid c)))
                             (hash-keys grid)))
  (define scores (map (lambda (c)
                        (trailhead-score grid c))
                      trailheads))
  (foldl + 0 scores))

(define (trailhead-score grid origin)
  (define peaks (make-hash))
  (find-reachable-peaks grid origin peaks)
  (hash-count peaks))
  
(define (find-reachable-peaks grid origin already-found)
  (define required-height (+ 1 (hash-ref grid origin)))
  (define nexts (filter (lambda (c)
                          (equal? required-height (hash-ref grid c #f)))
                        (neighbors origin)))

  (cond [(equal? required-height 9)
         (for ([n nexts])
           (hash-set! already-found n #t))]
        [else
         (for ([n nexts])
           (find-reachable-peaks grid n already-found))]))

(define (neighbors c)
  (list (coord (- (coord-x c) 1) (coord-y c))
        (coord (+ (coord-x c) 1) (coord-y c))
        (coord (coord-x c) (- (coord-y c) 1))
        (coord (coord-x c) (+ (coord-y c) 1))))
                  

(define (trailheads grid)
  (filter (lambda (c)
            (equal? (hash-ref grid c) 0))
          (hash-keys grid)))

(define grid (parse-input))
(score grid)