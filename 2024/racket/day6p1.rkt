#lang racket

(require "aoc.rkt")

(struct Coord (x y) #:inspector #f)
(struct Guard (pos orientation) #:inspector #f)
(struct Grid (size cells) #:inspector #f) ; grids are square

(define (coord-add-x c dx)
  (Coord (+ (Coord-x c) dx) (Coord-y c)))

(define (coord-add-y c dy)
  (Coord (Coord-x c) (+ (Coord-y c) dy)))

(define (parse-input)
  (define input (port->string (current-input-port)))
  (define lines (string-split input "\n"))
  (define cells (make-hash))
  (define y 0)
  (for ([line lines])
    (for ([x (string-length line)])
      (hash-set! cells (Coord x y) (string-ref line x))
      )
    (set! y (+ y 1)))
  (Grid y cells))

(define (solve grid)
  (define guard-pos (grid-find #\^ grid))
  (cond [(false? guard-pos) (raise "Could not find initial guard position")])
  (define guard (Guard guard-pos #\^))
  (path-length grid guard (hash Guard-pos #t)))

(define (grid-find needle haystack)
  (define result #f)
  ; TODO: How to short-circuit?
  (for ([y (Grid-size haystack)])
    (for ([x (Grid-size haystack)])
      (cond [(eq? needle (hash-ref (Grid-cells haystack) (Coord x y)))
             (set! result (Coord x y))])))
  result)

(define (path-length grid guard visited)
  (cond [(out-of-bounds? grid (Guard-pos guard))
         (- (hash-count visited) 1)]
        [else
         (define next-guard (advance guard grid))
         (path-length grid next-guard (hash-set visited (Guard-pos guard) #t))]))

(define (advance guard grid)
  (define pos (Guard-pos guard))
  (define next-pos (match (Guard-orientation guard)
                     [#\^ (coord-add-y pos -1)]
                     [#\> (coord-add-x pos 1)]
                     [#\v (coord-add-y pos 1)]
                     [#\< (coord-add-x pos -1)]
                     [_ (raise (string-append "Unexpected guard orientation: "
                                              (Guard-orientation guard)))]))
  (cond [(blocked? grid next-pos)
         (advance (turn guard) grid)]
        [else
         (Guard next-pos (Guard-orientation guard))]))

(define (turn guard)
  (define next-orientation (match (Guard-orientation guard)
                             [#\^ #\>]
                             [#\> #\v]
                             [#\v #\<]
                             [#\< #\^]
                             [_ (raise (string-append "Unexpected guard orientation: "
                                                      (Guard-orientation guard)))]))
  (Guard (Guard-pos guard) next-orientation))

(define (blocked? grid pos)
  (eq? #\# (hash-ref (Grid-cells grid) pos #f)))

(define (out-of-bounds? grid pos)
  (define x (Coord-x pos))
  (define y (Coord-y pos))
  (or
   (< x 0)
   (< y 0)
   (>= x (Grid-size grid))
   (>= y (Grid-size grid))))


(define grid (parse-input))
(solve grid)