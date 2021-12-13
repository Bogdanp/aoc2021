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
  (define proc
    (match instr
      [`(col ,fc) (λ (r c) (cons r (if (< c fc) c (- fc (- c fc)))))]
      [`(row ,fr) (λ (r c) (cons (if (< r fr) r (- fr (- r fr))) c))]))
  (for/hash ([(pos v) (in-hash ds)])
    (define row (car pos))
    (define col (cdr pos))
    (values (proc row col) v)))

(define (interp ds instrs)
  (for/fold ([ds ds])
            ([instr (in-list instrs)])
    (fold ds instr)))

(define (display-dots ds)
  (for ([row (in-inclusive-range 0 (apply max (map car (hash-keys ds))))])
    (for ([col (in-inclusive-range 0 (apply max (map cdr (hash-keys ds))))])
      (define pos (cons row col))
      (if (hash-has-key? ds pos)
          (display #\#)
          (display #\.)))
    (newline)))

(define part1 (time (hash-count (fold dots (car folds)))))
(define part2 (time (with-output-to-string
                      (λ () (display-dots (interp dots folds))))))

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
