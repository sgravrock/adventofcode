#lang racket

(require "aoc.rkt")

(struct mul (a b) #:inspector #f)
(struct do () #:inspector #f)
(struct dont () #:inspector #f)

(define mul-tokenizer (cons #rx"mul\\(([0-9]+),([0-9]+)\\)"
                            (lambda (captures)
                              (mul (string->number (car captures))
                                   (string->number (cadr captures))))))
(define do-tokenizer (cons #rx"do\\(\\)"
                           (lambda (captures) (do))))
(define dont-tokenizer (cons #rx"don't\\(\\)"
                             (lambda (captures) (dont))))

(define (tokenize-program input)
  (tokenize (list mul-tokenizer do-tokenizer dont-tokenizer) input))

(define (tokenize tokenizers input)
  (define first-match (first-tokenizer-result tokenizers input))
  (cond [(not first-match) '()]
        [else (define token (car first-match))
              (define rest-start (cdr first-match))
              (cons token (tokenize tokenizers (substring input rest-start)))]))


; tokenizers is a list of (regex . converter) pairs
; Each converter takes one argument, the strings captured by the regexp match
; Returns a (token . index of first un-consumed input char) pair
(define (first-tokenizer-result tokenizers input)
  (define results (filter-map (lambda (tokenizer)
                                (define re (car tokenizer))
                                (define m (regexp-match-positions re input))
                                (cond [(false? m)
                                       #f]
                                      [else
                                       (define converter (cdr tokenizer))
                                       (define match (regexp-match-positions (car tokenizer) input))
                                       (cons match converter)]))
                              tokenizers))
  (cond [(empty? results) #f]
        [else
         ; Grab the result with the first start position
         (define first-result (first-by (lambda (r)
                                          (define match (car r))
                                          (car (car match)))
                                        results))
         (define matches (car first-result))
         (define converter (cdr first-result))
         (define captures (regexp-match->captures (cdr matches) input))
         (define rest-start-ix (cdr (car matches)))
         (cons (converter captures) rest-start-ix)]))
 
(define (execute instructions)
  (define unskipped (filter-skipped #f instructions))
  (define products (map (lambda (ins)
                    (* (mul-a ins) (mul-b ins)))
                  unskipped))
  (foldl + 0 products))

(define (filter-skipped skipping instructions)
  (cond [(empty? instructions) '()]
        [(do? (car instructions)) (filter-skipped #f (cdr instructions))]
        [(dont? (car instructions)) (filter-skipped #t (cdr instructions))]
        [else (define rest (filter-skipped skipping (cdr instructions)))
              (cond [skipping rest]
                    [else (cons (car instructions) rest)])]))

(define program (tokenize-program (port->string (current-input-port))))
(execute program)