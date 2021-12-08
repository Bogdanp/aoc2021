#lang racket/base

(require racket/list
         racket/match
         racket/string)

(define data
  (call-with-input-file "day08.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match (string-split line " | ")
          [(list input output)
           (list
            (map string->list (string-split input))
            (map string->list (string-split output)))])))))

(define (simple-digit cs)
  (case (length cs)
    [(2) 1]
    [(4) 4]
    [(3) 7]
    [(7) 8]
    [else #f]))

(define (charset cs)
  (sort (remove-duplicates cs) char<?))

(define (charset-overlap? as bs)
  (for/and ([a (in-list as)])
    (memv a bs)))

(define (deduce-mapping css)
  (define m1 (for/first ([cs (in-list css)] #:when (= (length cs) 2)) cs))
  (define m4 (for/first ([cs (in-list css)] #:when (= (length cs) 4)) cs))
  (define m7 (for/first ([cs (in-list css)] #:when (= (length cs) 3)) cs))
  (define m8 (for/first ([cs (in-list css)] #:when (= (length cs) 7)) cs))
  (define m9 (for/first ([cs (in-list css)] #:when (and (= (length cs) 6) (charset-overlap? m4 cs))) cs))
  (define m0 (for/first ([cs (in-list css)] #:when (and (= (length cs) 6) (not (member cs (list m9))) (charset-overlap? m1 cs))) cs))
  (define m6 (for/first ([cs (in-list css)] #:when (and (= (length cs) 6) (not (member cs (list m0 m9))))) cs))
  (define m3 (for/first ([cs (in-list css)] #:when (and (= (length cs) 5) (charset-overlap? m1 cs))) cs))
  (define m5 (for/first ([cs (in-list css)] #:when (and (= (length cs) 5) (not (member cs (list m3))) (charset-overlap? cs m9))) cs))
  (define m2 (for/first ([cs (in-list css)] #:when (and (= (length cs) 5) (not (member cs (list m3 m5))))) cs))
  (list m0 m1 m2 m3 m4 m5 m6 m7 m8 m9))

(define part1
  (time
   (for*/sum ([d (in-list data)]
              [cs (in-list (cadr d))]
              #:when (simple-digit cs))
     1)))

(define part2
  (time
   (for/sum ([d (in-list data)])
     (define css (map charset (car d)))
     (define mapping (deduce-mapping css))
     (define digits
       (for/list ([cs (in-list (cadr d))])
         (index-of mapping (charset cs))))
     (define number
       (for/sum ([d (in-list digits)]
                 [e (in-range (sub1 (length digits)) -1 -1)])
         (* d (expt 10 e))))
     number)))

(module+ test
  (require rackunit)
  (check-= part1 470 0)
  (check-= part2 989396 0))
