#lang racket/base

(require racket/list
         racket/set)

(define data
  (call-with-input-file "day09.txt"
    (lambda (in)
      (for/fold ([points (hash)])
                 ([line (in-lines in)]
                  [i (in-naturals 1)])
        (for/fold ([points points])
                  ([c (in-string line)]
                   [j (in-naturals 1)])
          (define d (string->number (string c)))
          (hash-set points (cons i j) d))))))

(define (adjacent-positions pos)
  (define i (car pos))
  (define j (cdr pos))
  `((,(sub1 i) . ,j)
    (,i . ,(sub1 j))
    (,i . ,(add1 j))
    (,(add1 i) . ,j)))

(define (low-point? pos)
  (define p (hash-ref data pos #f))
  (and p (for/and ([q (in-list (adjacent-positions pos))])
           (< p (hash-ref data q +inf.0)))))

(define part1
  (for*/sum ([pos (in-hash-keys data)] #:when (low-point? pos))
    (add1 (hash-ref data pos))))

(define (basin-size pos)
  (define seen (mutable-set pos))
  (let loop ([pos pos])
    (define v (hash-ref data pos 9))
    (cond
      [(= v 9) 0]
      [else
       (add1
        (for/sum ([pos (in-list (adjacent-positions pos))]
                  #:unless (set-member? seen pos)
                  #:when (< v (hash-ref data pos -inf.0)))
          (set-add! seen pos)
          (loop pos)))])))

(define part2
  (time
   (let ([basins (for/list ([pos (in-hash-keys data)] #:when (low-point? pos))
                   (basin-size pos))])
     (apply * (take (sort basins >) 3)))))

(module+ test
  (require rackunit)
  (check-= part1 489 0)
  (check-= part2 1056330 0))
