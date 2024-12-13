#lang racket

(require "aoc.rkt")
(require threading)

(define (solve grid)
  (~> (all-regions grid)
      (map price _)
      (foldl + 0 _)))

(define (all-regions unused-plots)
  (cond [(hash-empty? unused-plots) '()]
        [else
         (define-values (region still-unused) (first-region unused-plots))
         (cons region (all-regions still-unused))]))

(define (price region)
  (* (area region) (perimeter region)))

(define (area region)
  (length region))

(define (perimeter region)
  (cond [(empty? region) 0]
        [(empty? (cdr region)) 4]
        [else (~> region
                  (map (lambda (plot)
                         (count-if (lambda(neighbor)
                                     (false? (member neighbor region)))
                                   (possible-neighbors plot)))
                       _)
                  (foldl + 0 _))]))

(define (contains? lst el)
  (not (false? member el lst)))

; Precondition: unused-plots is not empty
(define (first-region unused-plots)
  (define origin (car (hash-keys unused-plots)))
  (region-including origin unused-plots))


(define (region-including origin unused-plots)
  (cond [(not (hash-has-key? unused-plots origin))
         (values '() unused-plots)]
        [else
         (define crop (hash-ref unused-plots origin))
         (define updated-unused (hash-remove unused-plots origin))
         (define neighbors (filter (lambda (n)
                                     (equal? crop (hash-ref updated-unused n #f)))
                                   (possible-neighbors origin)))
         (for/fold ([reached (list origin)]
                    [unused updated-unused])
                   ([n neighbors])
           (define-values (sub-reached sub-unused) (region-including n unused))
           (values (append reached sub-reached) sub-unused))]))
    
(define (possible-neighbors c)
  (list (coord-+x c -1)
        (coord-+x c 1)
        (coord-+y c -1)
        (coord-+y c 1)))

(define grid (parse-grid))
(solve grid)