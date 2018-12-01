#lang racket

(define (fewest-steps directions)
  (let ([destination (follow-path '(0 0 0) directions)])
    (distance '(0 0 0) destination)))

; Flat-topped cubic coordinates
; See <https://www.redblobgames.com/grids/hexagons/>.
(define (neighbor from direction)
  (map sum (zip from (delta-for-direction direction))))

(define (delta-for-direction direction)
  (case direction
    [("n") '(0 1 -1)]
    [("ne") '(1 0 -1)]
    [("se") '(1 -1 0)]
    [("s") '(0 -1 1)]
    [("sw") '(-1 0 1)]
    [("nw") '(-1 1 0)]
    [else (raise (list 'bad-direction direction))]))

(define (follow-path from directions)
  (foldl (lambda (dir pos) (neighbor pos dir)) from directions))

(define (distance a b)
  (let ([axis-deltas (map (compose abs difference) (zip a b))])
    (/ (sum axis-deltas) 2)))

(define (zip a b)
  (if (empty? a)
      '()
      (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

(define (sum xs)
  (foldl + 0 xs))

(define (difference xs)
  (foldl - 0 xs))

(provide fewest-steps neighbor follow-path distance zip)