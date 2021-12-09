#lang racket/base

(require racket/list
         racket/set)

(define-values (data max-i max-j)
  (call-with-input-file "day09.txt"
    (lambda (in)
      (for/fold ([points (hash)] [max-i #f] [max-j #f])
                 ([line (in-lines in)]
                  [i (in-naturals 1)])
        (define-values (new-points max-j)
          (for/fold ([points points] [max-j #f])
                    ([c (in-string line)]
                     [j (in-naturals 1)])
            (define d (string->number (string c)))
            (values (hash-set points (cons i j) d) j)))
        (values new-points i max-j)))))

(define (low-point? i j)
  (define p (hash-ref data (cons i j) #f))
  (define u (hash-ref data (cons (sub1 i) j) +inf.0))
  (define d (hash-ref data (cons (add1 i) j) +inf.0))
  (define l (hash-ref data (cons i (sub1 j)) +inf.0))
  (define r (hash-ref data (cons i (add1 j)) +inf.0))
  (and p (< p u) (< p d) (< p l) (< p r)))

(define part1
  (for*/sum ([i (in-inclusive-range 1 max-i)]
             [j (in-inclusive-range 1 max-j)]
             #:when (low-point? i j))
    (add1 (hash-ref data (cons i j)))))

(define (basin-size i j)
  (define pos (cons i j))
  (define seen (mutable-set pos))
  (let loop ([pos pos])
    (define v (hash-ref data pos 9))
    (cond
      [(= v 9) 0]
      [else
       (define i (car pos))
       (define j (cdr pos))
       (add1
        (for/sum ([pos (in-list
                        `((,(sub1 i) . ,j)
                          (,i . ,(sub1 j))
                          (,i . ,(add1 j))
                          (,(add1 i) . ,j)))]
                  #:unless (set-member? seen pos)
                  #:when (< v (hash-ref data pos -inf.0)))
          (set-add! seen pos)
          (loop pos)))])))

(define part2
  (time
   (let ([basins (for*/list ([i (in-inclusive-range 1 max-i)]
                             [j (in-inclusive-range 1 max-j)]
                             #:when (low-point? i j))
                   (basin-size i j))])
     (apply * (take (sort basins >) 3)))))

(module+ test
  (require rackunit)
  (check-= part1 489 0)
  (check-= part2 1056330 0))
