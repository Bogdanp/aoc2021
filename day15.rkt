#lang racket/base

(require data/heap
         racket/fixnum)

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
        (for/fold ([grid (make-fxvector (hash-count grid-hash))])
                  ([(pos cost) (in-hash grid-hash)])
          (define row (car pos))
          (define col (cdr pos))
          (begin0 grid
            (fxvector-set! grid (fx+ col (fx* row M)) cost))))
      (values grid M))))

(define (moves pos stride)
  (list
   (fx- pos stride)
   (fx+ pos stride)
   (if (fx< (modulo pos stride) (fx- stride 1)) (fx+ pos 1) -1)
   (if (fx> (modulo pos stride) 0)              (fx- pos 1) -1)))

(define (a* start goal stride cost)
  (define candidates (make-heap fx<))
  (heap-add! candidates start)
  (define from (make-hasheqv))
  (define g-score (make-hasheqv `((,start . 0))))
  (define f-score (make-hasheqv `((,start . 0))))
  (let loop ()
    (when (zero? (heap-count candidates))
      (error 'a* "failed to find a path"))
    (define current (fxand (heap-min candidates) #xFFFFFFFF))
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
           (define s (fx+ t-score (cost neighbor)))
           (hash-set! from neighbor current)
           (hash-set! g-score neighbor t-score)
           (hash-set! f-score neighbor s)
           (heap-add! candidates (fxior (fxlshift s 32) neighbor))))
       (loop)])))

(define (total-risk path cost)
  (for/sum ([pos (in-list (cdr path))])
    (cost pos)))

(define (part1-cost pos)
  (cond
    [(or (fx<  pos 0)
         (fx>= pos (* M M)))
     +inf.0]
    [else
     (fxvector-ref grid pos)]))

(define M5 (fx* M 5))
(define MM (fx* M M))
(define MM55 (fx* M M 5 5))
(define pos2cost
  (for/fxvector #:length MM55 ([pos (in-range MM55)])
    (define row (fxquotient pos M5))
    (define col (fxmodulo pos M5))
    (define row* (fxmodulo row M))
    (define col* (fxmodulo col M))
    (define cost (fxvector-ref grid (fx+ col* (fx* row* M))))
    (define r (fx+ (fxquotient row M)
                   (fxquotient col M)
                   cost))
    (if (fx> r 9) (fx- r 9) r)))
(define (part2-cost pos)
  (cond
    [(or (fx<  pos 0)
         (fx>= pos MM55))
     +inf.0]
    [else
     (fxvector-ref pos2cost pos)]))

(define part1 (time (total-risk (a* 0 (sub1 MM)   M  part1-cost) part1-cost)))
(define part2 (time (total-risk (a* 0 (sub1 MM55) M5 part2-cost) part2-cost)))

(module+ test
  (require rackunit)
  (check-= part1 595 0)
  (check-= part2 2914 0))
