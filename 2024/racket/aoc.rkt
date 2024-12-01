#lang racket

(provide read-input)

(define (read-input line-re captures->rec [port (current-input-port)])
  (map (lambda (line)
         (captures->rec (regexp-captures line-re line)))
       (readlines port)))

(define (regexp-captures needle haystack)
  (define m (regexp-match-positions needle haystack))
  (cond [m (sequence->list (map (lambda(capture)
                                  (substring haystack (car capture) (cdr capture)))
                                (cdr m)))]
        [else #f]))

(define (readlines port)
  (define lines (string-split (port->string port) "\n"))
  (filter non-empty-string? lines)) ; esp. to take care of trailing blank lines
