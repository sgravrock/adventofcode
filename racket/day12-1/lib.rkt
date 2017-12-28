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

(define (debug-if result debug)
  (cond [result (debug)])
  result)

(define (all-that-reach dest-id lines)
  (let ([src-ids (map car lines)])
    (printf "src IDs: ~a\n" src-ids)
    (filter (lambda (src-id) (reaches src-id dest-id lines)) src-ids)))

(define (reaches src-id dest-id lines)
  (or (equal? src-id dest-id)
      (any? (lambda (line)
              (debug-if
               (reaches-via line src-id dest-id lines)
               (lambda ()
                 (printf "Top: ~a is reachable from ~a via line ~a\n" dest-id src-id line))))
            lines)))

(define (reaches-directly line dest-id)
  (debug-if (equal? dest-id (car line))
            (lambda () (printf "~a is directly reachable via line ~a\n" dest-id line))))

(define (reaches-via start-line src-id dest-id lines)
  (and (line-has-source? start-line src-id)
       (let ([candidate-lines (remq start-line lines)])
         (or (reaches-directly start-line dest-id)
             (debug-if
              (reaches-indirectly (car start-line) dest-id candidate-lines)
              (lambda ()
                (printf "~a is indirectly reachable from ~a" dest-id (car start-line))))))))

(define (reaches-indirectly src-id dest-id lines)
  (any? (lambda (line)
          (reaches-via line src-id dest-id lines))
        lines))

(define (line-has-source? line source-id)
  (not (equal? #f (member source-id (cdr line)))))

(define (find-line id lines)
  (findf (lambda (line)
           (equal? id (car line)))
         lines))

(provide parse-line reaches reaches-directly reaches-via find-line debug-if all-that-reach)