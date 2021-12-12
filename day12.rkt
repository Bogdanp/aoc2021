#lang racket/base

(require racket/match)

(define (add-edge edges a b)
  (hash-update edges a (λ (es) (cons b es)) null))

(define edges
  (call-with-input-file "day12.txt"
    (lambda (in)
      (for/fold ([edges (hasheq)])
                ([line (in-lines in)])
        (match line
          [(regexp #rx"([^-]+)-(.+)" (list _
                                           (app string->symbol a)
                                           (app string->symbol b)))
           (add-edge (add-edge edges a b) b a)])))))

(define small-cave?
  (let ([memo (make-hasheq)])
    (lambda (id)
      (hash-ref! memo id (λ ()
                           (define str (symbol->string id))
                           (string=? str (string-downcase str)))))))

(define (paths [allow-twice? #f])
  (let search ([path '(start)]
               [twice? (not allow-twice?)])
    (match path
      [(cons 'end _) (list path)]
      [(cons cave _)
       (apply append (for*/list ([c (in-list (hash-ref edges cave null))]
                                 #:unless (eq? c 'start)
                                 [small? (in-value (small-cave? c))]
                                 [visited? (in-value (memq c path))]
                                 #:when (or (not small?)
                                            (not visited?)
                                            (not twice?)))
                       (search (cons c path) (or twice? (and small? visited?)))))])))

(define part1 (time (length (paths))))
(define part2 (time (length (paths #t))))

(module+ test
  (require rackunit)
  (check-= part1 5457 0)
  (check-= part2 128506 0))
