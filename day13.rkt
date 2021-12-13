#lang racket/base

(require racket/match
         racket/port
         racket/string)

(define-values (dots folds)
  (call-with-input-file "day13.txt"
    (lambda (in)
      (define dots
        (for/hash ([line (in-lines in)])
          #:break (string=? line "")
          (match (string-split line ",")
            [(list (app string->number col) (app string->number row))
             (values (cons row col) 1)])))
      (define folds
        (for/list ([line (in-lines in)])
          (match line
            [(regexp #rx"fold along x=(.+)" (list _ (app string->number pos)))
             (list 'col pos)]
            [(regexp #rx"fold along y=(.+)" (list _ (app string->number pos)))
             (list 'row pos)])))
      (values dots folds))))

(define (fold ds instr)
  (match instr
    [`(col ,col-num)
     (for/hash ([(pos v) (in-hash ds)])
       (define row (car pos))
       (define col (cdr pos))
       (if (< col col-num)
           (values pos v)
           (values (cons row (- col-num (- col col-num))) v)))]
    [`(row ,row-num)
     (for/hash ([(pos v) (in-hash ds)])
       (define row (car pos))
       (define col (cdr pos))
       (if (< row row-num)
           (values pos v)
           (values (cons (- row-num (- row row-num)) col) v)))]))

(define part1
  (time
   (hash-count (fold dots (car folds)))))

(define (display-dots ds)
  (define mr (add1 (apply max (map car (hash-keys ds)))))
  (define mc (add1 (apply max (map cdr (hash-keys ds)))))
  (for ([row (in-range mr)])
    (for ([col (in-range mc)])
      (define pos (cons row col))
      (if (hash-has-key? ds pos)
          (display #\#)
          (display #\.)))
    (newline)))

(define part2
  (time
   (with-output-to-string
     (lambda ()
       (display-dots
        (for/fold ([ds dots])
                  ([instr (in-list folds)])
          (fold ds instr)))))))

(module+ test
  (require rackunit)
  (check-= part1 631 0)
  (check-equal? (string-trim part2) #<<CODE
####.####.#....####...##..##..###..####
#....#....#....#.......#.#..#.#..#.#...
###..###..#....###.....#.#....#..#.###.
#....#....#....#.......#.#.##.###..#...
#....#....#....#....#..#.#..#.#.#..#...
####.#....####.#.....##...###.#..#.#...
CODE
                ))
