#lang racket/base

(require racket/match)

(struct line (x1 y1 x2 y2)
  #:transparent)

(define (sign n)
  (cond
    [(= n 0) 0]
    [(> n 0) 1]
    [(< n 0) -1]))

;; in 1d space
(define (line-points l M [ok? (λ (_dx _dy) #t)])
  (match-define (line x1 y1 x2 y2) l)
  (define dx (sign (- x2 x1)))
  (define dy (sign (- y2 y1)))
  (if (ok? dx dy)
      (for/list ([i (in-inclusive-range 0 (max (abs (- x2 x1))
                                               (abs (- y2 y1))))])
        (define x (+ x1 (* i dx)))
        (define y (+ y1 (* i dy)))
        (+ x (* y M)))
      null))

(define-values (M lines)
  (call-with-input-file "day05.txt"
    (lambda (in)
      (for/fold ([M 0]
                 [lines null]
                 #:result (values M (reverse lines)))
                ([str (in-lines in)])
        (match str
          [(regexp #rx"([^,]+),([^ ]+) -> ([^,]+),(.+)"
                   (list _
                         (app string->number x1)
                         (app string->number y1)
                         (app string->number x2)
                         (app string->number y2)))
           (values
            (max M x1 y1 x2 y2)
            (cons (line x1 y1 x2 y2) lines))])))))

(define (solution points-proc)
  (define (count-dangerous points)
    (for/sum ([cnt (in-hash-values points)] #:when (> cnt 1)) 1))
  (for*/fold ([points (make-hasheqv)] #:result (count-dangerous points))
             ([l (in-list lines)]
              [p (in-list (points-proc l))])
    (begin0 points
      (hash-update! points p add1 0))))

(define part1 (solution (λ (l) (line-points l M (λ (dx dy) (or (zero? dx) (zero? dy)))))))
(define part2 (solution (λ (l) (line-points l M))))

(module+ test
  (require rackunit)
  (check-= part1 5442 0)
  (check-= part2 19571 0))
