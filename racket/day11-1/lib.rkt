#lang racket

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
    [else (raise 'bad-direction)]))

(define (zip a b)
  (if (empty? a)
      '()
      (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

(define (sum xs)
  (foldr + 0 xs))

(provide neighbor zip)