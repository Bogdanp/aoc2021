#lang racket/base

(require racket/set)

(define data
  (call-with-input-file "day11.txt"
    (lambda (in)
      (for/fold ([octopi (hash)])
                ([line (in-lines in)]
                 [i (in-naturals)])
        (for/fold ([octopi octopi])
                  ([c (in-string line)]
                   [j (in-naturals)])
          (hash-set octopi (cons i j) (string->number (string c))))))))

(define (adjacent-positions pos)
  (define i (car pos))
  (define j (cdr pos))
  `((,(sub1 i) . ,(sub1 j))
    (,(sub1 i) . ,j)
    (,(sub1 i) . ,(add1 j))
    (,i . ,(sub1 j))
    (,i . ,(add1 j))
    (,(add1 i) . ,(sub1 j))
    (,(add1 i) . ,j)
    (,(add1 i) . ,(add1 j))))

(define (step octopi)
  (define flashed (mutable-set))
  (define increased
    (for/hash ([(pos energy) (in-hash octopi)])
      (values pos (add1 energy))))
  (let loop ([octopi increased] [flashes 0])
    (define-values (new-octopi new-flashes)
      (for/fold ([octopi octopi] [flashes 0])
                ([(pos energy) (in-hash octopi)]
                 #:when (> energy 9)
                 #:unless (set-member? flashed pos))
        (set-add! flashed pos)
        (define flashed-octopi
          (for/fold ([octopi (hash-set octopi pos 0)])
                    ([other-pos (in-list (adjacent-positions pos))]
                     #:when (hash-has-key? octopi other-pos)
                     #:unless (set-member? flashed other-pos))
            (hash-update octopi other-pos add1)))
        (values flashed-octopi (add1 flashes))))
    (if (> new-flashes 0)
        (loop new-octopi (+ flashes new-flashes))
        (values new-octopi flashes (= (set-count flashed)
                                      (hash-count octopi))))))

(define part1
  (time
   (for/fold ([octopi data] [flashes 0] #:result flashes)
             ([_ (in-range 100)])
     (define-values (new-octopi new-flashes _all-flashed?)
       (step octopi))
     (values new-octopi (+ flashes new-flashes)))))

(define part2
  (time
   (for/fold ([octopi data] [all-step #f] #:result all-step)
             ([step-num (in-naturals 1)])
     (define-values (new-octopi _ all-flashed?)
       (step octopi))
     #:final all-flashed?
     (values new-octopi (and all-flashed? step-num)))))

(module+ test
  (require rackunit)
  (check-= part1 1755 0)
  (check-= part2 212 0))
