#lang racket/base

(require data/heap)

(define-values (grid M)
  (call-with-input-file "day15.txt"
    (lambda (in)
      (define grid-hash
        (for/fold ([grid (hash)])
                  ([line (in-lines in)]
                   [row (in-naturals)])
          (for/fold ([grid grid])
                    ([c (in-string line)]
                     [col (in-naturals)])
            (hash-set grid (cons row col) (string->number (string c))))))

      (define M (sqrt (hash-count grid-hash)))
      (define grid
        (for/fold ([grid (make-vector (hash-count grid-hash))])
                  ([(pos cost) (in-hash grid-hash)])
          (define row (car pos))
          (define col (cdr pos))
          (begin0 grid
            (vector-set! grid (+ col (* row M)) cost))))
      (values grid M))))

(define (moves pos stride)
  (list
   (- pos stride)
   (+ pos stride)
   (if (< (modulo pos stride) (sub1 stride)) (add1 pos) -1)
   (if (> (modulo pos stride) 0)             (sub1 pos) -1)))

(define (a* start goal stride cost)
  (define candidates (make-heap (λ (a b) (< (cdr a) (cdr b)))))
  (heap-add! candidates (cons start 0))
  (define from (make-hasheqv))
  (define g-score (make-hasheqv `((,start . 0))))
  (define f-score (make-hasheqv `((,start . 0))))
  (let loop ()
    (when (zero? (heap-count candidates))
      (error 'a* "failed to find a path"))
    (define current (car (heap-min candidates)))
    (cond
      [(= current goal)
       (let path-loop ([path (list current)] [current current])
         (define next (hash-ref from current #f))
         (if next (path-loop (cons next path) next) path))]
      [else
       (heap-remove-min! candidates)
       (for ([neighbor (in-list (moves current stride))])
         (define t-score (+ (hash-ref g-score current)
                            (+ (cost neighbor)
                               (cost current))))
         (when (< t-score (hash-ref g-score neighbor +inf.0))
           (define s (+ t-score (cost neighbor)))
           (hash-set! from neighbor current)
           (hash-set! g-score neighbor t-score)
           (hash-set! f-score neighbor s)
           (heap-add! candidates (cons neighbor s))))
       (loop)])))

(define (total-risk path h)
  (for/sum ([pos (in-list (cdr path))])
    (h pos)))

(define (part1-cost pos)
  (cond
    [(or (<  pos 0)
         (>= pos (* M M)))
     +inf.0]
    [else
     (vector-ref grid pos)]))

(define (part2-cost pos)
  (cond
    [(or (<  pos 0)
         (>= pos (* M M 5 5)))
     +inf.0]
    [else
     (define row (quotient pos (* M 5)))
     (define col (modulo pos (* M 5)))
     (define row* (modulo row M))
     (define col* (modulo col M))
     (define cost (vector-ref grid (+ col* (* row* M))))
     (define r (+ (quotient row M)
                  (quotient col M)
                  cost))
     (if (> r 9) (- r 9) r)]))

(define part1 (time (total-risk (a* 0 (sub1 (* M M))        M    part1-cost) part1-cost)))
(define part2 (time (total-risk (a* 0 (sub1 (* M M 5 5)) (* M 5) part2-cost) part2-cost)))

(module+ test
  (require rackunit)
  (check-= part1 595 0)
  (check-= part2 2914 0))
