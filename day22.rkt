#lang racket/base

(require racket/list
         racket/match)

(define instructions
  (call-with-input-file "day22.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match line
          [(regexp #rx"(on|off) x=([^.]+)\\.\\.([^,]+),y=([^.]+)\\.\\.([^,]+),z=([^.]+)\\.\\.(.+)"
                   (list _
                         (app string->symbol state)
                         (app string->number x-lo)
                         (app string->number x-hi)
                         (app string->number y-lo)
                         (app string->number y-hi)
                         (app string->number z-lo)
                         (app string->number z-hi)))
           (list state
                 (cons x-lo x-hi)
                 (cons y-lo y-hi)
                 (cons z-lo z-hi))])))))

(define (intersect a b)
  (define c
    (for/list ([r1 (in-list a)]
               [r2 (in-list b)]
               #:unless (> (car r1) (cdr r2))
               #:unless (< (cdr r1) (car r2)))
      (cons
       (max (car r1) (car r2))
       (min (cdr r1) (cdr r2)))))
  (and (= (length c) 3) c))

(define (difference a b)
  (define i (intersect a b))
  (cond
    [(not i) (list a)]
    [else
     (match-define (list ar1 ar2 ar3) a)
     (match-define (list ir1 ir2 ir3) i)
     (define cubes
       (list
        (list ar1 ar2 (cons (car ar3) (sub1 (car ir3))))
        (list ar1 ar2 (cons (add1 (cdr ir3)) (cdr ar3)))
        (list (cons (car ar1) (sub1 (car ir1))) ar2 ir3)
        (list (cons (add1 (cdr ir1)) (cdr ar1)) ar2 ir3)
        (list ir1 (cons (car ar2) (sub1 (car ir2))) ir3)
        (list ir1 (cons (add1 (cdr ir2)) (cdr ar2)) ir3)))
     (filter-map
      (λ (c)
        (match-define (list r1 r2 r3) c)
        (and
         (<= (car r1) (cdr r1))
         (<= (car r2) (cdr r2))
         (<= (car r3) (cdr r3))
         c))
      cubes)]))

(define (init instrs)
  (let loop ([cubes null]
             [instrs instrs])
    (cond
      [(null? instrs) cubes]
      [else
       (define instr (car instrs))
       (define cube (cdr instr))
       (define cubes*
         (apply append (for/list ([c (in-list cubes)])
                         (difference c cube))))
       (case (car instr)
         [(on) (loop (append cubes* (list cube)) (cdr instrs))]
         [(off) (loop cubes* (cdr instrs))])])))

(define cubes
  (init instructions))

(define (clamp-lo v) (min  50 v))
(define (clamp-hi v) (max -50 v))
(define part1
  (for/sum ([c (in-list cubes)])
    (match-define (list r1 r2 r3) c)
    (* (max 0 (add1 (- (clamp-lo (cdr r1)) (clamp-hi (car r1)))))
       (max 0 (add1 (- (clamp-lo (cdr r2)) (clamp-hi (car r2)))))
       (max 0 (add1 (- (clamp-lo (cdr r3)) (clamp-hi (car r3))))))))

(define part2
  (for/sum ([c (in-list cubes)])
    (match-define (list r1 r2 r3) c)
    (* (add1 (- (cdr r1) (car r1)))
       (add1 (- (cdr r2) (car r2)))
       (add1 (- (cdr r3) (car r3))))))

(module+ test
  (require rackunit)
  (check-= part1 580012 0)
  (check-= part2 1334238660555542 0))
