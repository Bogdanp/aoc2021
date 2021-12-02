#lang racket/base

(require racket/match)

(struct pos (horizontal depth aim)
  #:transparent)

(define (pos-horizontal+ p n [with-aim? #f])
  (struct-copy pos p
               [horizontal (+ (pos-horizontal p) n)]
               [depth (if with-aim?
                          (+ (pos-depth p) (* (pos-aim p) n))
                          (pos-depth p))]))

(define (pos-depth+ p n [with-aim? #f])
  (if with-aim?
      (struct-copy pos p [aim (+ (pos-aim p) n)])
      (struct-copy pos p [depth (+ (pos-depth p) n)])))

(define (pos-move p instr [with-aim? #f])
  (match instr
    [`(forward ,n) (pos-horizontal+ p n with-aim?)]
    [`(down ,n) (pos-depth+ p n with-aim?)]
    [`(up ,n) (pos-depth+ p (- n) with-aim?)]))

(define instrs
  (call-with-input-file "day02.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (define m (regexp-match #rx"([^ ]+) (.+)" line))
        (define i (string->symbol (cadr m)))
        (define n (string->number (caddr m)))
        `(,i ,n)))))

(define part1
  (for/fold ([p (pos 0 0 0)]
             #:result (* (pos-horizontal p)
                         (pos-depth p)))
            ([i (in-list instrs)])
    (pos-move p i)))

(define part2
  (for/fold ([p (pos 0 0 0)]
             #:result (* (pos-horizontal p)
                         (pos-depth p)))
            ([i (in-list instrs)])
    (pos-move p i #t)))

(module+ test
  (require rackunit)
  (check-= part1 1936494 0)
  (check-= part2 1997106066 0))
