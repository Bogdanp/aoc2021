#lang racket/base

(require racket/fixnum
         racket/match)

(struct line (x1 y1 x2 y2)
  #:transparent)

(define (fxsign n)
  (cond
    [(fx= n 0) 0]
    [(fx> n 0) 1]
    [(fx< n 0) -1]))

;; in 1d space
(define (line-points l M [ok? (λ (_dx _dy) #t)])
  (match-define (line x1 y1 x2 y2) l)
  (define dx (fxsign (- x2 x1)))
  (define dy (fxsign (- y2 y1)))
  (if (ok? dx dy)
      (for/list ([i (in-inclusive-range 0 (fxmax (fxabs (fx- x2 x1))
                                                 (fxabs (fx- y2 y1))))])
        (define x (fx+ x1 (fx* i dx)))
        (define y (fx+ y1 (fx* i dy)))
        (fx+ x (fx* y M)))
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
  (define points (make-fxvector (+ M (* M M)) 0))
  (for* ([l (in-list lines)]
         [p (in-list (points-proc l))])
    (fxvector-set! points p (fx+ 1 (fxvector-ref points p))))
  (for/sum ([n (in-fxvector points)] #:when (fx> n 1)) 1))

(define part1 (time (solution (λ (l) (line-points l M (λ (dx dy) (or (fx= dx 0) (fx= dy 0))))))))
(define part2 (time (solution (λ (l) (line-points l M)))))

(module+ test
  (require rackunit)
  (check-= part1 5442 0)
  (check-= part2 19571 0))
