#lang racket/base

(require racket/match)

(define-values (table image)
  (call-with-input-file "day20.txt"
    (lambda (in)
      (define enhancement-table (read-line in))
      (void (read-line in))
      (define image
        (for/fold ([image (hash)])
                  ([line (in-lines in)]
                   [row (in-naturals)])
          (for/fold ([image image])
                    ([chr (in-string line)]
                     [col (in-naturals)]
                     #:when (char=? chr #\#))
            (hash-set image (cons row col) chr))))
      (values enhancement-table image))))

(define (pos->output image posn [default #\.])
  (match-define (cons row col) posn)
  (define str
   (apply string
          (for*/list ([row (in-inclusive-range (sub1 row) (add1 row))]
                      [col (in-inclusive-range (sub1 col) (add1 col))]
                      [chr (in-value (hash-ref image (cons row col) default))])
            (case chr
              [(#\#) #\1]
              [(#\.) #\0]))))
  (string-ref table (string->number str 2)))

(define (step image [default #\.])
  (define M (apply max (append (map car (hash-keys image))
                               (map cdr (hash-keys image)))))
  (define new-image
    (for*/fold ([output (hash)])
               ([row (in-inclusive-range -1 (+ M 1))]
                [col (in-inclusive-range -1 (+ M 1))])
      (define pos (cons row col))
      (define chr (pos->output image pos default))
      (hash-set output (cons (add1 row) (add1 col)) chr)))
  (values new-image (if (char=? default #\.) #\# #\.)))

(define (enhance image steps)
  (let loop ([steps steps]
             [image image]
             [default #\.])
    (cond
      [(zero? steps) image]
      [else
       (define-values (new-image new-default)
         (step image default))
       (loop (sub1 steps) new-image new-default)])))

(define (display-image image)
  (define m (apply min (append (map car (hash-keys image))
                               (map cdr (hash-keys image)))))
  (define M (apply max (append (map car (hash-keys image))
                               (map cdr (hash-keys image)))))
  (for ([row (in-inclusive-range m M)])
    (for ([col (in-inclusive-range m M)])
      (define pos (cons row col))
      (display (hash-ref image pos #\.)))
    (newline)))

(define part1 (time (for/sum ([chr (in-hash-values (enhance image 2))]  #:when (char=? chr #\#)) 1)))
(define part2 (time (for/sum ([chr (in-hash-values (enhance image 50))] #:when (char=? chr #\#)) 1)))

(module+ test
  (require rackunit)
  (check-= part1 5765 0)
  (check-= part2 18509 0))
