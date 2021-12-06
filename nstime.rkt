#lang racket/base

(require monotonic)
(provide nstime)

(define-syntax-rule (nstime e0 e ...)
  (let ([st (nanotime)])
    (begin0 (let () e0 e ...)
      (let ([duration (- (nanotime) st)])
        (if (> duration 1000)
            (printf "took: ~aμs~n" (quotient duration 1000))
            (printf "took: ~ans~n" duration))))))
