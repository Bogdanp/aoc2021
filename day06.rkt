#lang racket/base

(require racket/string
         "nstime.rkt")

(define the-fish
  (call-with-input-file "day06.txt"
    (lambda (in)
      (for/fold ([fish (hasheqv)])
                ([str (string-split (read-line in) "," #:repeat? #t)])
        (hash-update fish (string->number str) add1 0)))))

(define (step fish)
  (for*/fold ([res (hasheqv)])
             ([age (in-inclusive-range 0 8)]
              [n (in-value (hash-ref fish age #f))]
              #:when n)
    (if (zero? age)
        (hash-set (hash-set res 8 n) 6 n)
        (hash-update res (sub1 age) (λ (v) (+ v n)) 0))))

(define (interp fish n)
  (for/fold ([fish fish] #:result (apply + (hash-values fish)))
            ([_ (in-range n)])
    (step fish)))

(define part1 (nstime (interp the-fish 80)))
(define part2 (nstime (interp the-fish 256)))

(module+ test
  (require rackunit)
  (check-= part1 386640 0)
  (check-= part2 1733403626279 0))
