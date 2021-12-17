#lang racket/base

(require racket/match)

(define-values (x-min x-max y-min y-max)
  (call-with-input-file "day17.txt"
    (lambda (in)
      (match (read-line in)
        [(regexp #rx"target area: x=([^.]+)..([^,]+), y=([^.]+)..(.+)"
                 (list _
                       (app string->number x-min)
                       (app string->number x-max)
                       (app string->number y-min)
                       (app string->number y-max)))
         (values x-min x-max y-min y-max)]))))

(struct probe (x y max-y vx vy) #:transparent)

(define (make-probe vx vy)
  (probe 0 0 0 vx vy))

(define (step p)
  (match p
    [(probe x y max-y vx vy)
     (define y* (+ y vy))
     (probe (+ x vx)
            y*
            (if (> y* max-y) y* max-y)
            (cond
              [(zero? vx) 0]
              [(> vx 0) (sub1 vx)]
              [else (add1 vx)])
            (sub1 vy))]))

(define (interp p)
  (let loop ([p p])
    (case (status p)
      [(hit) p]
      [(fail) #f]
      [else (loop (step p))])))

(define (status p)
  (match-define (probe x y _ _ _) p)
  (cond
    [(and (>= x x-min)
          (<= x x-max)
          (>= y y-min)
          (<= y y-max))
     'hit]
    [(or (> x x-max)
         (< y y-min))
     'fail]
    [else
     'miss]))

(define mx (max (abs x-min) (abs x-max)))
(define my (max (abs y-min) (abs y-max)))
(define solutions
  (time
   (for*/list ([vx (in-inclusive-range (- mx) mx)]
               [vy (in-inclusive-range (- my) my)]
               [p (in-value (interp (make-probe vx vy)))]
               #:when p)
     (list (probe-max-y p) vx vy))))

(define part1 (caar (sort solutions #:key car >)))
(define part2 (length solutions))

(module+ test
  (require rackunit)
  (check-= part1 5778 0)
  (check-= part2 2576 0))
