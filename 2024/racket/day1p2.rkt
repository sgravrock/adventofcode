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

(define (similarity-score n right-list)
  (define count (length (filter (lambda (x) (= n x)) right-list)))
  (* n count))

(define lists (sequence->list (unzip (parse-lines (readlines)))))
(define left (car lists))
(define right (cadr lists))
(foldl + 0 (map (lambda (x) (similarity-score x right)) left))