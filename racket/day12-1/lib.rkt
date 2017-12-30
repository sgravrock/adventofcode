#lang racket

(define (parse-line line)
  (string-split
   (string-replace
    (string-replace line "," "")
    "<-> "
    "")
   " "))

(define (any? predicate list)
  (not (equal? #f (findf predicate list))))

(define (contains? needle haystack)
  (not (equal? #f (member needle haystack))))

(define (member-containing needle haystack)
  (let ([result (memf (lambda (lst) (member needle lst)) haystack)])
  (if (equal? #f result)
      #f
      (car result))))

(define (all-that-reach dest-id lines)
  (member-containing dest-id (find-groups lines)))

(define (find-groups lines)
  (find-groups2 lines '()))

(define (find-groups2 lines groups)
  (if (equal? 0 (length lines))
      groups
      (find-groups2 (cdr lines) (add-to-groups (car lines) groups))))

(define (add-to-groups line groups)
  (let-values ([(matches non-matches)
                (partition (lambda (group) (line-touches-group? line group)) groups)])
    (let* ([matches-with-line (cons line matches)]
           [merged-matches (set-union-all matches-with-line)])
      (append (list merged-matches) non-matches))))

(define (line-touches-group? line group)
  (any? (lambda (id) (contains? id group)) line))

(define (set-union-all sets)
  (apply set-union sets))

(provide parse-line all-that-reach find-groups set-union-all member-containing)