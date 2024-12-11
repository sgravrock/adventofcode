#lang racket

(require "aoc.rkt")
(require threading)

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

(define (rating grid)
  (~> (hash-keys grid)
      (filter (lambda (c) (equal? 0 (hash-ref grid c)))
              _)
      (map (lambda (c) (trailhead-rating grid c))
           _)
      (foldl + 0 _)))

(define (trailhead-rating grid origin)
  (define required-height (+ 1 (hash-ref grid origin)))
  (define nexts (filter (lambda (c)
                          (equal? required-height (hash-ref grid c #f)))
                        (neighbors origin)))
  (cond [(equal? required-height 9)
         (length nexts)]
        [else
         (~> nexts
             (map (lambda (n) (trailhead-rating grid n))
                  _)
             (foldl + 0 _))]))

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
(rating grid)