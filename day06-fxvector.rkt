#lang racket/base

(require racket/fixnum
         racket/string
         "nstime.rkt")

(define the-fish
  (call-with-input-file "day06.txt"
    (lambda (in)
      (define fish (make-fxvector 9))
      (for ([str (string-split (read-line in) "," #:repeat? #t)])
        (define age (string->number str))
        (fxvector-set! fish age (fx+ 1 (fxvector-ref fish age))))
      fish)))

(define (step! fish)
  (define n (fxvector-ref fish 0))
  (for* ([age (in-range 0 8)])
    (fxvector-set! fish age (fxvector-ref fish (fx+ age 1))))
  (fxvector-set! fish 6 (+ (fxvector-ref fish 6) n))
  (fxvector-set! fish 8 n))

(define (interp fish n)
  (let ([fish (fxvector-copy fish)])
    (for ([_ (in-range n)])
      (step! fish))
    (for/sum ([n (in-fxvector fish)])
      n)))

(define part1 (nstime (interp the-fish 80)))
(define part2 (nstime (interp the-fish 256)))

(module+ test
  (require rackunit)
  (check-= part1 386640 0)
  (check-= part2 1733403626279 0))
