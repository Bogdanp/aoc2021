#lang racket/base

(require racket/match)

(define (nextpos p r)
  (add1 (modulo (sub1 (+ p r)) 10)))

(module+ part1
  (struct die (rolls state c) #:transparent #:mutable)

  (define (make-die)
    (die 0 0 0))

  (define (roll d)
    (set-die-state! d (add1 (die-state d)))
    (cond
      [(> (die-state d) 100)
       (set-die-state! d 0)
       (set-die-c! d 0)
       (roll d)]
      [else
       (define c* (add1 (die-c d)))
       (begin0 c*
         (set-die-rolls! d (add1 (die-rolls d)))
         (set-die-c! d c*))]))

  (define (roll* d)
    (for/sum ([_ (in-range 3)])
      (roll d)))

  (define (play p1 p2)
    (define d (make-die))
    (let loop ([p1 p1]
               [p2 p2]
               [p1score 0]
               [p2score 0]
               [p2score* 0])
      (define rolls (die-rolls d))
      (cond
        [(>= p1score 1000) (* (- rolls 3) p2score*)]
        [(>= p2score 1000) (* rolls p1score)]
        [else
         (define p1m (roll* d))
         (define p2m (roll* d))
         (define p1pos (nextpos p1 p1m))
         (define p2pos (nextpos p2 p2m))
         (loop p1pos p2pos (+ p1score p1pos) (+ p2score p2pos) p2score)])))

  (define part1
    (time (play 4 2)))

  (module+ test
    (require rackunit)
    (check-= part1 908595 0)))

(module+ part2
  (define rolls
    (for*/list ([r1 (in-list '(1 2 3))]
                [r2 (in-list '(1 2 3))]
                [r3 (in-list '(1 2 3))])
      (+ r1 r2 r3)))


  (define (play p1 p2)
    (define memo (make-hash))
    (define (turn p1 p2 p1pos p2pos)
      (define k (list p1 p2 p1pos p2pos))
      (cond
        [(>= p1 21) (list 1 0)]
        [(>= p2 21) (list 0 1)]
        [(hash-ref memo k #f)]
        [else
         (define res
           (for/fold ([s (list 0 0)])
                     ([r (in-list rolls)])
             (define pos (nextpos p1pos r))
             (match-define (list p1s p2s) s)
             (match-define (list p2s* p1s*) (turn p2 (+ p1 pos) p2pos pos))
             (list (+ p1s p1s*) (+ p2s p2s*))))
         (begin0 res
           (hash-set! memo k res))]))
    (turn 0 0 p1 p2))

  (define part2
    (time (apply max (play 4 2))))

  (module+ test
    (require rackunit)
    (check-= part2 91559198282731 0)))
