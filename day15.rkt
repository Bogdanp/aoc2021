#lang racket/base

(require racket/list)

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
   (if (< (modulo pos stride) stride) (add1 pos) -1)
   (if (> (modulo pos stride) 0)      (sub1 pos) -1)))

(define (remove-index lst idx)
  (append (take lst idx)
          (drop lst (add1 idx))))

(define (insert-candidate xs x-scores v v-score)
  (let loop ([xs xs]
             [x-scores x-scores]
             [ys null]
             [y-scores null])
    (cond
      [(null? xs)
       (values
        (reverse (cons v ys))
        (reverse (cons v-score y-scores)))]
      [else
       (define x (car xs))
       (define x-score (car x-scores))
       (if (< v-score x-score)
           (values
            (append (reverse ys) (cons v xs))
            (append (reverse y-scores) (cons v-score x-scores)))
           (loop (cdr xs) (cdr x-scores) (cons x ys) (cons x-score y-scores)))])))

(define (a* start goal stride cost)
  (define from (make-hasheqv))
  (define g-score (make-hasheqv `((,start . 0))))
  (define f-score (make-hasheqv `((,start . 0))))
  (let loop ([candidates (list start)]
             [candidate-scores (list 0)])
    (when (null? candidates)
      (error 'a* "failed to find a path"))
    (define current (car candidates))
    (cond
      [(= current goal)
       (let path-loop ([path (list current)] [current current])
         (define next (hash-ref from current #f))
         (if next (path-loop (cons next path) next) path))]
      [else
       (define-values (new-candidates new-candidate-scores)
         (for/fold ([candidates (remv current candidates)]
                    [candidate-scores (remove-index candidate-scores (index-of candidates current))])
                   ([neighbor (in-list (moves current stride))])
           (define t-score (+ (hash-ref g-score current)
                              (+ (cost neighbor)
                                 (cost current))))
           (cond
             [(>= t-score (hash-ref g-score neighbor +inf.0))
              (values candidates candidate-scores)]
             [else
              (define s (+ t-score (cost neighbor)))
              (hash-set! from neighbor current)
              (hash-set! g-score neighbor t-score)
              (hash-set! f-score neighbor s)
              (cond
                [(memv neighbor candidates)
                 (values candidates candidate-scores)]
                [else
                 (insert-candidate candidates candidate-scores neighbor s)])])))
       (loop new-candidates new-candidate-scores)])))

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
