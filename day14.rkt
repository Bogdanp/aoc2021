#lang racket/base

(#%declare #:unsafe)

(require racket/fixnum
         racket/match)

(define (elt->int elt)
  (- (char->integer elt) 65))

(define-values (polymer rules)
  (call-with-input-file "day14.txt"
    (lambda (in)
      (define polymer
        (map elt->int (string->list (read-line in))))
      (read-line in)
      (define rules
        (for/hash ([line (in-lines in)])
          (match line
            [(regexp #rx"([^ ]+) -> (.)"
                     (list _
                           (app (compose1 (λ (pair) (map elt->int pair)) string->list) pair)
                           (app (compose1 elt->int car string->list) element)))
             (values pair (list (list (car pair) element)
                                (list element (cadr pair))))])))
      (values polymer rules))))

(define (step p n)
  (define start-sequence
    (for/list ([a (in-list p)]
               [b (in-list (cdr p))])
      (list a b)))
  (define counts (make-fxvector 26))
  (fxvector-set! counts (car p) 1)
  (define memo (make-hash))
  (begin0 counts
    (let loop ([sequence start-sequence] [n n])
      (define k (list n sequence))
      (cond
        [(hash-ref memo k #f)
         => (λ (to-incrby)
              (begin0 to-incrby
                (for ([(elt amt) (in-hash to-incrby)])
                  (fxvector-set! counts elt (fx+ amt (fxvector-ref counts elt))))))]
        [(zero? n)
         (define to-incrby
           (for/fold ([to-incrby (hasheqv)])
                     ([pair (in-list sequence)])
             (define elt (cadr pair))
             (fxvector-set! counts elt (fx+ 1 (fxvector-ref counts elt)))
             (hash-update to-incrby elt add1 0)))
         (begin0 to-incrby
           (hash-set! memo k to-incrby))]
        [else
         (define to-incrby
           (for*/fold ([to-incrby (hasheqv)])
                      ([pair (in-list sequence)]
                       [(elt amt) (in-hash (loop (hash-ref rules pair) (sub1 n)))])
             (hash-update to-incrby elt (λ (v) (+ amt v)) 0)))
         (begin0 to-incrby
           (hash-set! memo k to-incrby))]))))

(define (solution steps)
  (define counts (step polymer steps))
  (define min-count (for/fold ([c +inf.0]) ([v (in-fxvector counts)] #:unless (zero? v)) (if (< v c) v c)))
  (define max-count (for/fold ([c      0]) ([v (in-fxvector counts)]) (if (> v c) v c)))
  (- max-count min-count))

(define part1 (time (solution 10)))
(define part2 (time (solution 40)))

(module+ test
  (require rackunit)
  (check-= part1 2170 0)
  (check-= part2 2422444761283 0))
