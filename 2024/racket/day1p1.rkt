#lang racket

(require seq)


; Returns a pair of lists of numers, one list for each column,
; with the numbers in no particular order 
(define (parse-lines lines)
  (define re #rx"^([0-9]+)   ([0-9]+)$")
  (map (lambda (line)
         (map string->number (regexp-captures re line)))
       lines))

(define (readlines)
  (define lines (string-split (port->string (current-input-port)) "\n"))
  (filter non-empty-string? lines)) ; esp. to take care of trailing blank lines

(define (regexp-captures needle haystack)
  (define m (regexp-match-positions needle haystack))
  (cond [m (map (lambda(capture)
                  (substring haystack (car capture) (cdr capture)))
                (cdr m))]
        [else #f]))

(define lists (unzip (parse-lines (readlines))))
(define sorted-lists (sequence->list (map (lambda (lst)
                                            (sort lst <))
                                          lists)))
(define pairs (zip (car sorted-lists) (cadr sorted-lists)))
(define diffs (map (lambda (pair)
                     (define a (car pair))
                     (define b (cadr pair))
                     (cond [(< a b) (- b a)]
                           [else (- a b)]))
                   pairs))
(foldl + 0 diffs)