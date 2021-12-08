#lang racket/base

(require racket/list
         racket/match
         racket/set
         racket/string)

(define data
  (call-with-input-file "day08.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match (string-split line " | ")
          [(list input output)
           (list
            (map (compose1 list->seteqv string->list) (string-split input))
            (map (compose1 list->seteqv string->list) (string-split output)))])))))

(define (deduce-mapping css)
  (define (first-where valid?-proc)
    (for/first ([cs (in-list css)] #:when (valid?-proc cs)) cs))
  (define m1 (first-where (λ (cs) (= (set-count cs) 2))))
  (define m4 (first-where (λ (cs) (= (set-count cs) 4))))
  (define m7 (first-where (λ (cs) (= (set-count cs) 3))))
  (define m8 (first-where (λ (cs) (= (set-count cs) 7))))
  (define m9 (first-where (λ (cs) (and (= (set-count cs) 6) (subset? m4 cs)))))
  (define m0 (first-where (λ (cs) (and (= (set-count cs) 6) (not (member cs (list m9))) (subset? m1 cs)))))
  (define m6 (first-where (λ (cs) (and (= (set-count cs) 6) (not (member cs (list m0 m9)))))))
  (define m3 (first-where (λ (cs) (and (= (set-count cs) 5) (subset? m1 cs)))))
  (define m5 (first-where (λ (cs) (and (= (set-count cs) 5) (not (member cs (list m3))) (subset? cs m9)))))
  (define m2 (first-where (λ (cs) (and (= (set-count cs) 5) (not (member cs (list m3 m5)))))))
  (list m0 m1 m2 m3 m4 m5 m6 m7 m8 m9))

(define part1
  (time
   (for*/sum ([d (in-list data)]
              [cs (in-list (cadr d))]
              #:when (memv (set-count cs) '(2 4 3 7)))
     1)))

(define part2
  (time
   (for/sum ([d (in-list data)])
     (define mapping (deduce-mapping (car d)))
     (define digits
       (for/list ([cs (in-list (cadr d))])
         (index-of mapping cs)))
     (for/sum ([d (in-list (reverse digits))]
               [e (in-naturals 0)])
       (* d (expt 10 e))))))

(module+ test
  (require rackunit)
  (check-= part1 470 0)
  (check-= part2 989396 0))
