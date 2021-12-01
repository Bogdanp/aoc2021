#lang racket/base

(require racket/list)

(define (count-increases ns)
  (for*/fold ([c 0]
              [p +inf.0]
              #:result c)
             ([n (in-list ns)])
    (values (if (< p n) (add1 c) c) n)))

(define nums
  (call-with-input-file "day01.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

(define part1
  (count-increases nums))

(define part2
  (count-increases
   (for/fold ([sums null]
              [window null]
              #:result (reverse (cons (apply + window) sums)))
              ([n (in-list nums)])
     (if (= (length window) 3)
         (values (cons (apply + window) sums) (cons n (take window 2)))
         (values sums (cons n window))))))

(module+ test
  (require rackunit)
  (check-= part1 1557 0)
  (check-= part2 1608 0))
