#lang racket/base

(require racket/match)

(define-values (polymer rules)
  (call-with-input-file "day14.txt"
    (lambda (in)
      (define polymer
        (string->list (read-line in)))
      (void (read-line in))
      (define rules
        (for/hash ([line (in-lines in)])
          (match line
            [(regexp #rx"([^ ]+) -> (.)"
                     (list _
                           (app string->list pair)
                           (app (compose1 car string->list) element)))
             (values pair (list (list (car pair) element)
                                (list element (cadr pair))))])))
      (values polymer rules))))

(define-syntax-rule (define/memo (name arg ...) body0 body ...)
  (define name
    (let ([memo (make-hash)])
      (lambda (arg ...)
        (define k (list arg ...))
        (cond
          [(hash-ref memo k #f)]
          [else (let ([res (begin body0 body ...)])
                  (begin0 res
                    (hash-set! memo k res)))])))))

(define/memo (count sequence n)
  (if (zero? n)
      (for/fold ([counts (hasheqv)])
                ([pair (in-list sequence)])
        (define elt (cadr pair))
        (hash-update counts elt add1 0))
      (for*/fold ([counts (hasheqv)])
                 ([pair (in-list sequence)]
                  [(elt amt) (in-hash (count (hash-ref rules pair) (sub1 n)))])
        (hash-update counts elt (λ (v) (+ amt v)) 0))))

(define (step p n)
  (define start-sequence
    (for/list ([a (in-list p)]
               [b (in-list (cdr p))])
      (list a b)))
  (define counts (count start-sequence n))
  (hash-update counts (car p) add1 0))

(define (solution steps)
  (define counts (step polymer steps))
  (define min-count (apply min (hash-values counts)))
  (define max-count (apply max (hash-values counts)))
  (- max-count min-count))

(define part1 (time (solution 10)))
(define part2 (time (solution 40)))

(module+ test
  (require rackunit)
  (check-= part1 2170 0)
  (check-= part2 2422444761283 0))
