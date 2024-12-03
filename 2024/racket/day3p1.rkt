#lang racket

(define (all-captures needle haystack)
  (define m (regexp-match-positions needle haystack))
  (cond [(not m) '()]
        [else
         (define captures (map (lambda(capture)
                                  (substring haystack (car capture) (cdr capture)))
                                (cdr m)))
         (define rest (all-captures needle (substring haystack (cdr (car m)))))
         (cond [(empty? rest) (list captures)]
               [else (cons captures rest)])]))

(define (parse-input input)
  (map (lambda (pair)
         (map string->number pair))
       (all-captures #rx"mul\\(([0-9]+),([0-9]+)\\)" (without-blank-lines input))))

(define (without-blank-lines s)
  (string-join (filter (lambda (line)
                       (not (eq? line "")))
                       (string-split s "\n"))
               "\n"))

(define instructions (parse-input (port->string (current-input-port))))
(foldl + 0 (map (lambda (pair)
                (* (car pair) (cadr pair)))
              instructions))