#lang racket/base

(require racket/list
         racket/match
         racket/string)

(define data
  (call-with-input-file "day08.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match (string-split line " | ")
          [(list input output)
           (list
            (map string->list (string-split input))
            (map string->list (string-split output)))])))))

(define segments-to-digits
  (hash
   '(#\a #\b #\c #\e #\f #\g) 0
   '(#\c #\f) 1
   '(#\a #\c #\d #\e #\g) 2
   '(#\a #\c #\d #\f #\g) 3
   '(#\b #\c #\d #\f) 4
   '(#\a #\b #\d #\f #\g) 5
   '(#\a #\b #\d #\e #\f #\g) 6
   '(#\a #\c #\f) 7
   '(#\a #\b #\c #\d #\e #\f #\g) 8
   '(#\a #\b #\c #\d #\f #\g) 9))

(define (simple-digit cs)
  (case (length cs)
    [(2) 1]
    [(4) 4]
    [(3) 7]
    [(7) 8]
    [else #f]))

(define (digit cs)
  (hash-ref
   segments-to-digits
   (remove-duplicates (sort cs char<?))
   #f))

(define (complex-number css)
  (for/sum ([d (in-list (map digit css))]
            [e (in-range (sub1 (length css)) -1 -1)])
    (* d (expt 10 e))))

(define (set- as bs)
  (for/list ([v (in-list as)] #:unless (memv v bs)) v))

(define (deduce-mapping css)
  (define simple-mapping
    (for*/hasheqv ([cs (in-list css)]
                   [n (in-value (simple-digit cs))]
                   #:when n)
      (values n (sort cs char<?))))
  (define m1 (hash-ref simple-mapping 1))
  (define m4 (hash-ref simple-mapping 4))
  (define m7 (hash-ref simple-mapping 7))
  (define m8 (hash-ref simple-mapping 8))
  (define aaaa (car (set- m7 m1)))
  (define bbbb-or-dddd (set- m4 m1))
  (define cccc-or-ffff m1)
  (define eeee-or-gggg (remv aaaa (set- m8 m4)))
  (define candidate-mappings
    (hash
     aaaa '(#\a . #\a)
     (car  bbbb-or-dddd) '(#\b . #\d)
     (cadr bbbb-or-dddd) '(#\d . #\b)
     (car  cccc-or-ffff) '(#\c . #\f)
     (cadr cccc-or-ffff) '(#\f . #\c)
     (car  eeee-or-gggg) '(#\e . #\g)
     (cadr eeee-or-gggg) '(#\g . #\e)))
  ;; cs -> list of candidate digits
  (define candidates
    (for/hash ([cs (in-list css)] #:unless (simple-digit cs))
      (values cs (let loop ([cs (remove-duplicates (sort cs char<?))]
                            [rs null])
                   (cond
                     [(null? cs)
                      (remove-duplicates
                       (filter-map digit rs))]
                     [else
                      (define c (car cs))
                      (define cands
                        (hash-ref candidate-mappings c))
                      (loop (cdr cs)
                            (if (null? rs)
                                (list
                                 (list (car cands))
                                 (list (cdr cands)))
                                (append
                                 (for/list ([r (in-list rs)])
                                   (cons (car cands) r))
                                 (for/list ([r (in-list rs)])
                                   (cons (cdr cands) r)))))])))))
  ;; matched digits -> cs
  (define matches
    (let loop ([candidates candidates])
      (cond
        [(andmap (λ (ds) (= (length ds) 1)) (hash-values candidates))
         (for/hasheqv ([(cs cands) (in-hash candidates)])
           (values (car cands) cs))]
        [else
         (define exact-matches
           (for/hash ([(cs cands) (in-hash candidates)] #:when (= 1 (length cands)))
             (values cands cs)))
         (define exact-candidates
           (for/hash ([(cands cs) (in-hash exact-matches)])
             (values cs cands)))
         (define partial-matches
           (for/hash ([(cs cands) (in-hash candidates)] #:when (> (length cands) 1))
             (values (filter-not (λ (d) (hash-has-key? exact-matches (list d))) cands) cs)))
         (loop
          (for/fold ([candidates exact-candidates])
                    ([(cands cs) (in-hash partial-matches)])
            (hash-set candidates cs cands)))])))
  (define m0 (hash-ref matches 0))
  (define m2 (hash-ref matches 2))
  (define m5 (hash-ref matches 5))
  (define m9 (hash-ref matches 9))
  (define cccc (car (set- m1 m5)))
  (define ffff (car (remv cccc cccc-or-ffff)))
  (define dddd (car (set- m9 m0)))
  (define bbbb (car (remv dddd bbbb-or-dddd)))
  (define eeee (car (set- m2 m9)))
  (define gggg (car (remv eeee eeee-or-gggg)))
  (hash
   aaaa #\a
   bbbb #\b
   cccc #\c
   dddd #\d
   eeee #\e
   ffff #\f
   gggg #\g))

(define part1
  (time
   (for*/sum ([d (in-list data)]
              [cs (in-list (cadr d))]
              #:when (simple-digit cs))
     1)))

(define part2
  (time
   (for/sum ([d (in-list data)])
     (define mapping (deduce-mapping (apply append d)))
     (define mapped-output-digits
       (for/list ([cs (in-list (cadr d))])
         (for/list ([c (in-list cs)])
           (hash-ref mapping c))))
     (complex-number mapped-output-digits))))

(module+ test
  (require rackunit)
  (check-= part1 470 0)
  (check-= part2 989396 0))
