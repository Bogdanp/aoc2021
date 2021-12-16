#lang racket/base

(require racket/match)

(define data
  (call-with-input-file "day16.txt"
    (lambda (in)
      (apply
       string-append
       (for/list ([c (in-string (read-line in))])
         (case c
           [(#\0) "0000"]
           [(#\1) "0001"]
           [(#\2) "0010"]
           [(#\3) "0011"]
           [(#\4) "0100"]
           [(#\5) "0101"]
           [(#\6) "0110"]
           [(#\7) "0111"]
           [(#\8) "1000"]
           [(#\9) "1001"]
           [(#\A) "1010"]
           [(#\B) "1011"]
           [(#\C) "1100"]
           [(#\D) "1101"]
           [(#\E) "1110"]
           [(#\F) "1111"]))))))

(struct packet (version type) #:transparent)
(struct literal packet (value) #:transparent)
(struct operator packet (packets) #:transparent)

(define (decode-one data [padded? #t])
  (match data
    [(regexp #rx"^(...)(...)" (list _ version-str type-str))
     (define version (string->number version-str 2))
     (define type (string->number type-str 2))
     (case type
       [(4)
        (let literal-loop ([bitss null]
                           [data (substring data 6)])
          (define chunk (substring data 0 5))
          (define more? (char=? #\1 (string-ref chunk 0)))
          (define bitss* (cons (substring chunk 1) bitss))
          (cond
            [more? (literal-loop bitss* (substring data 5))]
            [else
             (define value (string->number (apply string-append (reverse bitss*)) 2))
             (define pack (literal version type value))
             (values pack (if padded?
                              (substring data (+ 5 (length bitss*)))
                              (substring data 5)))]))]
       [else
        (case (string-ref data 6)
          [(#\0)
           (define subpack-len (string->number (substring data 7 22) 2))
           (define subpack-data (substring data 22 (+ 22 subpack-len)))
           (define subpacks (decode subpack-data #f))
           (define pack (operator version type subpacks))
           (values pack (if padded?
                            (substring data (* (ceiling (/ (+ 22 subpack-len) 8)) 8))
                            (substring data (+ 22 subpack-len))))]
          [(#\1)
           (define num-subpacks (string->number (substring data 7 18) 2))
           (define-values (subpacks rest-data)
             (for/fold ([data (substring data 18)]
                        [subpacks null]
                        #:result (values (reverse subpacks) data))
                       ([_ (in-range num-subpacks)])
               (define-values (subpack rest-data)
                 (decode-one data #f))
               (values rest-data (cons subpack subpacks))))
           (define pack (operator version type subpacks))
           (values pack (if padded?
                            ""
                            rest-data))])])]))

(define (decode data [padded? #t])
  (let loop ([data data]
             [packets null])
    (cond
      [(string=? data "")
       (reverse packets)]
      [else
       (define-values (pack rest-data)
         (decode-one data padded?))
       (loop rest-data (cons pack packets))])))

(define (version-sum p)
  (match p
    [(literal version _ _) version]
    [(operator version _ ps) (apply + version (map version-sum ps))]))

(define (interp p)
  (match p
    [(literal _ _ v) v]
    [(operator _ 0 ps) (apply + (map interp ps))]
    [(operator _ 1 ps) (apply * (map interp ps))]
    [(operator _ 2 ps) (apply min (map interp ps))]
    [(operator _ 3 ps) (apply max (map interp ps))]
    [(operator _ 5 ps) (if (> (interp (car ps)) (interp (cadr ps))) 1 0)]
    [(operator _ 6 ps) (if (< (interp (car ps)) (interp (cadr ps))) 1 0)]
    [(operator _ 7 ps) (if (= (interp (car ps)) (interp (cadr ps))) 1 0)]))

(define part1 (apply + (map version-sum (decode data))))
(define part2 (apply + (map interp (decode data))))

(module+ test
  (require rackunit)
  (check-= part1 951 0)
  (check-= part2 902198718880 0))
