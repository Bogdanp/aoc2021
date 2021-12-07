#lang racket/base

(require racket/string)

(define positions
  (call-with-input-file "day07.txt"
    (lambda (in)
      (map string->number (string-split (read-line in) "," #:repeat? #t)))))

(define (compute-costs poss cost-proc)
  (for*/fold ([costs (hasheqv)])
             ([target-pos (in-inclusive-range 0 (apply max poss))]
              [source-pos (in-list poss)])
    (hash-update costs target-pos (λ (v) (+ v (cost-proc target-pos source-pos))) 0)))

(define (solution poss [cost-proc (λ (t s) (abs (- t s)))])
  (for/fold ([res #f] [fuel +inf.0] #:result fuel)
            ([(pos cost) (in-hash (compute-costs poss cost-proc))])
    (if (< cost fuel)
        (values pos cost)
        (values res fuel))))

(define part1 (time (solution positions)))
(define part2 (time (solution positions
                              (let ([memo (make-hasheqv)])
                                (λ (t s)
                                  (define d (abs (- t s)))
                                  (hash-ref! memo d (λ ()
                                                      (for/sum ([s (in-inclusive-range 1 d)])
                                                        s))))))))

(module+ test
  (require rackunit)
  (check-= part1 348664 0)
  (check-= part2 100220525 0))
