#lang racket/base

(require racket/match)

(define grid
  (call-with-input-file "day25.txt"
    (lambda (in)
      (for/fold ([grid (hash)])
                ([line (in-lines in)]
                 [row (in-naturals 1)])
        (for/fold ([grid grid])
                  ([chr (in-string line)]
                   [col (in-naturals 1)])
          (if (char=? chr #\.)
              grid
              (hash-set grid (cons row col) chr)))))))

(define W (apply max (map cdr (hash-keys grid))))
(define H (apply max (map car (hash-keys grid))))

(define (step-east g)
  (for*/fold ([g* g])
             ([(p chr) (in-hash g)])
    (case chr
      [(#\>)
       (match-define (cons row col) p)
       (define np (cons row (add1 (modulo col W))))
       (case (hash-ref g np #\.)
         [(#\.) (hash-remove (hash-set g* np chr) p)]
         [else g*])]
      [else g*])))

(define (step-south g)
  (for/fold ([g* g])
            ([(p chr) (in-hash g)])
    (case chr
      [(#\v)
       (match-define (cons row col) p)
       (define np (cons (add1 (modulo row H)) col))
       (case (hash-ref g np #\.)
         [(#\.) (hash-remove (hash-set g* np chr) p)]
         [else g*])]
      [else g*])))

(define (step g)
  (step-south (step-east g)))

(define (display-map g)
  (for ([row (in-inclusive-range 1 H)])
    (for ([col (in-inclusive-range 1 W)])
      (display (hash-ref g (cons row col) #\.)))
    (newline)))

(define part1
  (time
   (let loop ([g grid] [steps 1])
     (define g* (step g))
     (if (equal? g g*)
         steps
         (loop g* (add1 steps))))))

(module+ test
  (require rackunit)
  (check-= part1 351 0))
