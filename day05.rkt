#lang racket/base

(require racket/match)

(struct line (x1 y1 x2 y2)
  #:transparent)

(define (horizontal-line? l) (= (line-x1 l) (line-x2 l)))
(define (vertical-line? l)   (= (line-y1 l) (line-y2 l)))
(define (line-direction l)
  (match-define (line x1 y1 x2 y2) l)
  (cond
    [(and (< x2 x1) (< y2 y1)) 'nw]
    [(and (< x2 x1) (> y2 y1)) 'sw]
    [(and (> x2 x1) (< y2 y1)) 'ne]
    [(and (> x2 x1) (> y2 y1)) 'se]
    [else (raise-argument-error 'l "a diagonal line" l)]))

(define (line-points l)
  (cond
    [(horizontal-line? l)
     (define x (line-x1 l))
     (for/list ([y (in-inclusive-range
                    (min (line-y1 l) (line-y2 l))
                    (max (line-y1 l) (line-y2 l)))])
       (cons x y))]

    [(vertical-line? l)
     (define y (line-y1 l))
     (for/list ([x (in-inclusive-range
                    (min (line-x1 l) (line-x2 l))
                    (max (line-x1 l) (line-x2 l)))])
       (cons x y))]

    [else
     (match-define (line x1 y1 x2 y2) l)
     (define dir (line-direction l))
     (define dist
       (min
        (abs (- x2 x1))
        (abs (- y2 y1))))
     (for/list ([n (in-inclusive-range 0 dist)])
       (define nx
         (case dir
           [(nw sw) (- n)]
           [(ne se) n]))
       (define ny
         (case dir
           [(nw ne) (- n)]
           [(sw se) n]))
       (cons (+ x1 nx)
             (+ y1 ny)))]))

(define lines
  (call-with-input-file "day05.txt"
    (lambda (in)
      (for/list ([str (in-lines in)])
        (match str
          [(regexp #rx"([^,]+),([^ ]+) -> ([^,]+),(.+)"
                   (list _
                         (app string->number x1)
                         (app string->number y1)
                         (app string->number x2)
                         (app string->number y2)))
           (line x1 y1 x2 y2)])))))

(define (count-dangerous points)
  (for/sum ([cnt (in-hash-values points)] #:when (>= cnt 2)) 1))

(define part1
  (for/fold ([points (hash)] #:result (count-dangerous points))
            ([l (in-list lines)]
             #:when (or (horizontal-line? l)
                        (vertical-line? l))
             [p (in-list (line-points l))])
    (hash-update points p add1 0)))

(define part2
  (for*/fold ([points (hash)] #:result (count-dangerous points))
             ([l (in-list lines)]
              [p (in-list (line-points l))])
    (hash-update points p add1 0)))

(module+ test
  (require rackunit)
  (check-= part1 5442 0)
  (check-= part2 19571 0))
