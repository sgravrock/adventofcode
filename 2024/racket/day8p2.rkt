#lang racket

(require "aoc.rkt")

(struct Coord (x y) #:inspector #f)

; cells and coords-by-contents are both mutable hashes
; cells is coord -> char
; coords-by-contents is char -> (hash of coord -> #t)
(struct Grid (cells coords-by-contents))

(define (Grid-add grid coord content)
  (hash-set! (Grid-cells grid) coord content)
  (define coords (hash-ref! (Grid-coords-by-contents grid) content make-hash))
  (hash-set! coords coord #t))

(define (Grid-contains? grid coord)
  (hash-has-key? (Grid-cells grid) coord))

(define (parse-input)
  (define grid (Grid (make-hash) (make-hash)))
  (define y 0)
  (define lines (readlines))
  (for ([line lines])
    (for ([x (string-length line)])
      (define c (string-ref line x))
      (define adjusted (cond [(equal? c #\#) #\.]
                             [else c]))
      (Grid-add grid (Coord x y) adjusted))
    (set! y (+ 1 y)))
  grid)

(define (solve grid)
  (define antinodes (make-hash))
  (for ([freq (hash-keys (Grid-coords-by-contents grid))])
    (cond [(not (equal? freq #\.))
           (define coords-with-this-freq
             (hash-keys (hash-ref (Grid-coords-by-contents grid) freq)))
           (for-each-unequal-pair (lambda (a b)
                                    (for-antinodes (lambda (antinode)
                                                     (hash-set! antinodes antinode #t))
                                                   a b grid))
                                  coords-with-this-freq)]))
  (length (hash-keys antinodes)))

(define (for-each-unequal-pair proc lst)
  (for ([a lst])
    (for ([b lst])
      (cond [(not (equal? a b)) (proc a b)]))))

(define (for-antinodes proc antenna1 antenna2 grid)
  (define (in-direction start dx dy)
    (define c (Coord (+ (Coord-x start) dx) (+ (Coord-y start) dy)))
    (cond [(Grid-contains? grid c)
           (proc c)
           (in-direction c dx dy)]))
  (define dx (- (Coord-x antenna1) (Coord-x antenna2)))
  (define dy (- (Coord-y antenna1) (Coord-y antenna2)))
  (in-direction antenna1 dx dy)
  (in-direction antenna2 (* dx -1) (* dy -1))
  (proc antenna1)
  (proc antenna2))

(define grid (parse-input))
(solve grid)