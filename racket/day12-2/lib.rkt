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

(define (find-groups lines)
  (find-groups2 lines '()))

(define (find-groups2 lines groups)
  (printf "find-groups2 ~a ~a\n" lines groups)
  (if (equal? 0 (length lines))
      groups
      (find-groups2 (cdr lines) (add-to-groups (car lines) groups))))

(define (add-to-groups line groups)
  (printf "add-to-groups ~a ~a\n" line groups)
  (let-values ([(matches non-matches)
                (partition (lambda (group) (line-touches-group? line group)) groups)])
    (let* ([matches-with-line (cons line matches)]
           [merged-matches (set-union-all matches-with-line)]
           [result (append (list merged-matches) non-matches)])
      (printf "matches-with-line: ~a\n" matches-with-line)
      (printf "merged-matches: ~a\n" merged-matches)
      (printf "result: ~a\n" result)
      result)))

(define (line-touches-group? line group)
  (printf "line-touches-group ~a ~a\n" line group)
  (any? (lambda (id) (contains? id group)) line))

(define (set-union-all sets)
  (apply set-union sets))


(provide parse-line find-groups set-union-all)