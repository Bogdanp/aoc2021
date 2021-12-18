#lang racket/base

(require racket/math
         racket/port
         racket/string)

(define (read-num s)
  (call-with-input-string (string-replace s "," " . ") read))

(define (write-num n)
  (cond
    [(number? n)
     (write n)]
    [else
     (display #\[)
     (write-num (car n))
     (display #\,)
     (write-num (cdr n))
     (display #\])]))

(define nums
  (call-with-input-file "day18.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (read-num line)))))

(struct node (parent left right) #:mutable #:transparent)
(struct pair node (a b) #:mutable #:transparent)
(struct num node (n) #:mutable #:transparent)

(define (node-left* n)
  (when (procedure? (node-left n))
    (set-node-left! n ((node-left n))))
  (node-left n))

(define (node-right* n)
  (when (procedure? (node-right n))
    (set-node-right! n ((node-right n))))
  (node-right n))

(define (node->num m)
  (if (num? m)
      (num-n m)
      (cons (node->num (pair-a m))
            (node->num (pair-b m)))))

(define (num->node n)
  (let loop ([parent #f] [n n])
    (cond
      [(number? n)
       (num parent #f #f n)]
      [else
       (define m (pair parent #f #f #f #f))
       (define a (loop m (car n)))
       (define b (loop m (cdr n)))
       (set-pair-a! m a)
       (set-pair-b! m b)
       (set-node-left!  b a)
       (set-node-right! a b)
       (set-node-left!  a (λ () (node-left*  m)))
       (set-node-right! b (λ () (node-right* m)))
       m])))

(define ((make-pair-adder accessor) m v)
  (let loop ([m m])
    (if (num? m)
        (set-num-n! m (+ (num-n m) v))
        (loop (accessor m)))))

(define pair-add-left!  (make-pair-adder pair-a))
(define pair-add-right! (make-pair-adder pair-b))

(define (pair-replace! p old-m new-m)
  (if (eq? (pair-a p) old-m)
      (set-pair-a! p new-m)
      (set-pair-b! p new-m)))

(define (try-explode! m)
  (let loop ([m m] [depth 0])
    (cond
      [(num? m) #f]
      [(= depth 4)
       (define p (node-parent m))
       (define a (pair-a m))
       (define b (pair-b m))
       (cond [(node-left*  m) => (λ (l) (pair-add-right! l (num-n a)))])
       (cond [(node-right* m) => (λ (r) (pair-add-left!  r (num-n b)))])
       (pair-replace! p m (num p (node-left* a) (node-right* a) 0))
       #t]
      [else
       (or (loop (pair-a m) (add1 depth))
           (loop (pair-b m) (add1 depth)))])))

(define (try-split! m)
  (let loop ([m m])
    (if (num? m)
        (cond
          [(> (num-n m) 9)
           (define p (node-parent m))
           (define num-a (num p (node-left* m) #f (exact-floor (/ (num-n m) 2))))
           (define num-b (num p #f (node-right* m) (exact-ceiling (/ (num-n m) 2))))
           (define new-m (pair p (node-left* m) (node-right* m) num-a num-b))
           (set-node-left!  num-b num-a)
           (set-node-right! num-a num-b)
           (pair-replace! p m new-m)
           #t]
          [else #f])
        (or (loop (pair-a m))
            (loop (pair-b m))))))

(define (num-step n)
  (define m (num->node n))
  (define exploded? (try-explode! m))
  (define split? (and (not exploded?) (try-split! m)))
  (values (or exploded? split?) (node->num m)))

(define num-add cons)
(define (num-reduce n)
  (define-values (reduced? new-n)
    (num-step n))
  (if reduced? (num-reduce new-n) n))

(define (num-magnitude n)
  (if (number? n)
      n
      (+ (* 3 (num-magnitude (car n)))
         (* 2 (num-magnitude (cdr n))))))

(define part1
  (time
   (num-magnitude
    (for/fold ([sum (car nums)])
              ([n (in-list (cdr nums))])
      (num-reduce (num-add sum n))))))

(define part2
  (time
   (for*/fold ([largest 0])
              ([a (in-list nums)]
               [b (in-list nums)]
               #:unless (eq? a b))
     (define m (num-magnitude (num-reduce (num-add a b))))
     (if (> m largest) m largest))))

(module+ test
  (require rackunit)
  (check-= part1 3494 0)
  (check-= part2 4712 0))
