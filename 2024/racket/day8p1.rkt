#lang racket

(require "aoc.rkt")

(struct Coord (x y) #:inspector #f)

(define (Coord-+ c delta)
  (Coord (+ (Coord-x c) (Coord-x delta))
         (+ (Coord-y c) (Coord-y delta))))

(define (Coord-- c delta)
  (Coord (- (Coord-x c) (Coord-x delta))
         (- (Coord-y c) (Coord-y delta))))

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
  (define delta (Coord (- (Coord-x antenna1) (Coord-x antenna2))
                       (- (Coord-y antenna1) (Coord-y antenna2))))
  (define candidates (list (Coord-+ antenna1 delta)
                           (Coord-- antenna2 delta)))
  (for ([c candidates])
    (cond [(Grid-contains? grid c) (proc c)])))

(define grid (parse-input))
(solve grid)