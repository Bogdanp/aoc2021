#lang racket/base

(require racket/file
         racket/list
         racket/match)

(define (corrupted-score c)
  (match c
    [#\) 3]
    [#\] 57]
    [#\} 1197]
    [#\> 25137]))

(define (read-chunk in)
  (let/ec return
    (let read-loop ()
      (define (read-pair open close)
        (let loop ([children null])
          (define c (peek-char in))
          (cond
            [(eof-object? c) (list open (reverse children) 'eof)]
            [(equal? c close) (list open (reverse children) (read-char in))]
            [(member c '(#\) #\] #\} #\>)) (return (corrupted-score (read-char in)))]
            [else (loop (cons (read-loop) children))])))
      (match (read-char in)
        [(? eof-object?) 'eof]
        [#\( (read-pair #\( #\))]
        [#\[ (read-pair #\[ #\])]
        [#\{ (read-pair #\{ #\})]
        [#\< (read-pair #\< #\>)]))))

(define lines (file->lines "day10.txt"))
(define part1
  (for/sum ([line (in-list lines)])
    (define maybe-score (read-chunk (open-input-string line)))
    (if (number? maybe-score) maybe-score 0)))

(define (incomplete-score c)
  (match c
    [`(,c ,children eof)
     (define score (add1 (index-of '(#\( #\[ #\{ #\<) c)))
     (+ score (* 5 (apply + (filter-map incomplete-score children))))]
    [_ #f]))

(define part2
  (let* ([scores (for*/list ([line (in-list lines)]
                             [line-in (in-value (open-input-string line))]
                             [chunk-or-score (in-value (read-chunk line-in))]
                             #:unless (number? chunk-or-score))
                   (define chunk
                     (let loop ([prev chunk-or-score])
                       (define curr (read-chunk line-in))
                       (if (eq? curr 'eof) prev (loop curr))))
                   (incomplete-score chunk))]
         [scores (sort (filter values scores) <)])
    (list-ref scores (quotient (length scores) 2))))

(module+ test
  (require rackunit)
  (check-= part1 370407 0)
  (check-= part2 3249889609 0))
