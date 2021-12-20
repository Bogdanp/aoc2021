#lang racket/base

(require racket/list
         racket/match
         racket/string)

(struct posn (x y z) #:transparent)

(define (make-posn x y [z 0])
  (posn x y z))

(define-syntax-rule (define-posn-ops [id op] ...)
  (begin
    (define (id a b)
      (match-define (posn ax ay az) a)
      (match-define (posn bx by bz) b)
      (posn (op ax bx) (op ay by) (op az bz))) ...))

(define-posn-ops
  [posn- -]
  [posn* *])

(define beacons-by-scanner
  (call-with-input-file "day19.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (unless (string-prefix? line "---")
          (error 'read "bad scanner line"))
        (for/list ([line (in-lines in)])
          #:break (string=? line "")
          (apply make-posn (map string->number (string-split line ","))))))))

(define rotations
  (list
   values
   (λ (p) (posn (posn-x p) (posn-z p) (posn-y p)))
   (λ (p) (posn (posn-y p) (posn-x p) (posn-z p)))
   (λ (p) (posn (posn-y p) (posn-z p) (posn-x p)))
   (λ (p) (posn (posn-z p) (posn-y p) (posn-x p)))
   (λ (p) (posn (posn-z p) (posn-x p) (posn-y p)))))
(define negations
  (list (posn  1  1  1)
        (posn  1  1 -1)
        (posn  1 -1  1)
        (posn  1 -1 -1)
        (posn -1  1  1)
        (posn -1  1 -1)
        (posn -1 -1  1)
        (posn -1 -1 -1)))

(define (adjust posns r neg)
  (for/list ([posn (in-list posns)])
    (posn* (r posn) neg)))
(define (find-xform a-posns b-posns [n 12])
  (define a-set
    (for/hash ([p (in-list a-posns)])
      (values p #t)))
  (let/ec return
    (for* ([r (in-list rotations)]
           [neg (in-list negations)])
      (define b*-posns (adjust b-posns r neg))
      (for* ([a-posn (in-list a-posns)]
             [b*-posn (in-list b*-posns)])
        (define xform (posn- b*-posn a-posn))
        (define-values (matches xformed)
          (for/fold ([matches 0]
                     [xformed null]
                     #:result (values matches (reverse xformed)))
                    ([c*-posn (in-list b*-posns)])
            (define a*-posn (posn- c*-posn xform))
            (values
             (if (hash-has-key? a-set a*-posn) (add1 matches) matches)
             (cons a*-posn xformed))))
        (when (>= matches n)
          (return xform xformed))))
    (values #f #f)))

(define scanners (length beacons-by-scanner))
(define scanners* (list->vector beacons-by-scanner))
(define xforms (list (posn 0 0 0)))
(define aligned (make-hasheqv (list (cons 0 (car beacons-by-scanner)))))
(define skip (make-hash))
(time
 (let loop ()
   (when (< (hash-count aligned) scanners)
     (for* ([i (in-range scanners)]
            #:unless (hash-has-key? aligned i)
            [j (in-hash-keys aligned)]
            #:unless (hash-has-key? skip (cons i j)))
       (println (list i j))
       (define-values (xform xformed)
         (find-xform
          (hash-ref aligned j)
          (vector-ref scanners* i)))
       (when xform
         (println `(match ,i))
         (set! xforms (cons xform xforms))
         (hash-set! aligned i xformed))
       #:break xform
       (hash-set! skip (cons i j) #f))
     (loop))))

(define part1
  (length (remove-duplicates (apply append (hash-values aligned)))))

(define part2
  (for*/fold ([greatest 0])
             ([a (in-list xforms)]
              [b (in-list xforms)])
    (match-define (posn ax ay az) a)
    (match-define (posn bx by bz) b)
    (define dist
      (+ (abs (- ax bx))
         (abs (- ay by))
         (abs (- az bz))))
    (if (> dist greatest) dist greatest)))

(module+ test
  (require rackunit)
  (check-= part1 442 0)
  (check-= part2 11079 0))
