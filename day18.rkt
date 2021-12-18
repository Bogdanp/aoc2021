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

(define numbers
  (call-with-input-file "day18.txt"
    (lambda (in)
      (for/list ([line (in-lines in)]) (read-num line)))))

(struct node (parent left right) #:mutable #:transparent)
(struct pair node (a b) #:mutable #:transparent)
(struct num node (n) #:mutable #:transparent)

(define (node->pair n)
  (cond
    [(num? n) (num-n n)]
    [else (cons (node->pair (pair-a n))
                (node->pair (pair-b n)))]))

(define (node-left* n)
  (when (procedure? (node-left n))
    (set-node-left! n ((node-left n))))
  (node-left n))

(define (node-right* n)
  (when (procedure? (node-right n))
    (set-node-right! n ((node-right n))))
  (node-right n))

(define ((make-adder accessor) m v)
  (let loop ([m m])
    (if (num? m)
        (set-num-n! m (+ (num-n m) v))
        (loop (accessor m)))))

(define add-left! (make-adder pair-a))
(define add-right! (make-adder pair-b))

(define (step n)
  (define m
    (let help ([parent #f] [n n])
      (cond
        [(number? n)
         (num parent #f #f n)]
        [else
         (define m (pair parent #f #f #f #f))
         (define a (help m (car n)))
         (define b (help m (cdr n)))
         (set-pair-a! m a)
         (set-pair-b! m b)
         (set-node-left! a (λ () (node-left* m)))
         (set-node-right! a b)
         (set-node-left! b a)
         (set-node-right! b (λ () (node-right* m)))
         m])))
  (define exploded?
    (let loop ([m m] [depth 0])
      (cond
        [(num? m) #f]
        [(= depth 4)
         (define p (node-parent m))
         (define a (pair-a m))
         (define b (pair-b m))
         (define l (node-left* m))
         (define r (node-right* m))
         (begin0 #t
           (when l (add-right! l (num-n a)))
           (when r (add-left! r (num-n b)))
           (if (eq? (pair-a p) m)
               (set-pair-a! p (num p (node-left* a) (node-right* a) 0))
               (set-pair-b! p (num p (node-left* b) (node-right* b) 0))))]
        [else
         (or (loop (pair-a m) (add1 depth))
             (loop (pair-b m) (add1 depth)))])))
  (define split?
    (and (not exploded?)
         (let loop ([m m])
           (cond
             [(and (num? m)
                   (> (num-n m) 9))
              (define p (node-parent m))
              (define new-m (pair p (node-left* m) (node-right* m) #f #f))
              (define num-a (num p #f #f (exact-floor (/ (num-n m) 2))))
              (define num-b (num p #f #f (exact-ceiling (/ (num-n m) 2))))
              (set-pair-a! new-m num-a)
              (set-pair-b! new-m num-b)
              (set-node-left! num-a (λ () (node-left* m)))
              (set-node-right! num-a num-b)
              (set-node-left! num-b num-a)
              (set-node-right! num-b (λ () (node-right* m)))
              (begin0 #t
                (if (eq? (pair-a p) m)
                    (set-pair-a! p new-m)
                    (set-pair-b! p new-m)))]
             [(num? m) #f]
             [else
              (or (loop (pair-a m))
                  (loop (pair-b m)))]))))
  (values (or exploded? split?) (node->pair m)))

(define add-nums cons)
(define (reduce-num n)
  (define-values (reduced? new-n)
    (step n))
  (if reduced? (reduce-num new-n) n))

(define (magnitude n)
  (if (number? n)
      n
      (+ (* 3 (magnitude (car n)))
         (* 2 (magnitude (cdr n))))))

(define part1
  (time
   (magnitude
    (for/fold ([sum (car numbers)])
              ([n (in-list (cdr numbers))])
      (reduce-num (add-nums sum n))))))

(define part2
  (time
   (for*/fold ([largest 0])
              ([a (in-list numbers)]
               [b (in-list numbers)]
               #:unless (eq? a b))
     (define m (magnitude (reduce-num (add-nums a b))))
     (if (> m largest) m largest))))

(module+ test
  (require rackunit)
  (check-= part1 3494 0)
  (check-= part2 4712 0))
