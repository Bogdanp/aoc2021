#lang racket/base

(define bitstrs
  (call-with-input-file "day03.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(define (most-common strs pos)
  (define counts
    (for*/fold ([counts (hash #\1 0 #\0 0)])
               ([str (in-list strs)]
                [chr (in-value (string-ref str pos))])
      (hash-update counts chr add1)))
  (define ones-count (hash-ref counts #\1))
  (define zeros-count (hash-ref counts #\0))
  (cond
    [(= ones-count zeros-count) #\2]
    [(> ones-count zeros-count) #\1]
    [else #\0]))

(define (least-common strs pos)
  (define mc (most-common strs pos))
  (cond
    [(equal? mc #\2) #\2]
    [(equal? mc #\1) #\0]
    [else #\1]))

(define (construct-bitstr strs method)
  (apply string (for/list ([pos (in-range (apply min (map string-length strs)))])
                  (method strs pos))))

(define gamma (string->number (construct-bitstr bitstrs most-common) 2))
(define epsilon (string->number (construct-bitstr bitstrs least-common) 2))
(define part1 (* gamma epsilon))

(define (reduce-rating strs method)
  (let loop ([posn 0]
             [strs strs])
    (if (= (length strs) 1)
        (car strs)
        (loop (add1 posn) (method posn strs)))))

(define oxygen-rating-bitstr
  (reduce-rating bitstrs (lambda (posn strs)
                           (define mc
                             (let ([mc (most-common strs posn)])
                               (if (equal? mc #\2) #\1 mc)))
                           (for*/list ([str (in-list strs)]
                                       [bit (in-value (string-ref str posn))]
                                       #:when (equal? bit mc))
                             str))))
(define oxygen-rating
  (string->number oxygen-rating-bitstr 2))

(define c02-scrubber-rating-bitstr
  (reduce-rating bitstrs (lambda (posn strs)
                           (define lc
                             (let ([mc (least-common strs posn)])
                               (if (equal? mc #\2) #\0 mc)))
                           (for*/list ([str (in-list strs)]
                                       [bit (in-value (string-ref str posn))]
                                       #:when (equal? bit lc))
                             str))))
(define c02-scrubber-rating
  (string->number c02-scrubber-rating-bitstr 2))
(define part2 (* oxygen-rating c02-scrubber-rating))

(module+ test
  (require rackunit)
  (check-= part1 2035764 0)
  (check-= part2 2817661 0))
