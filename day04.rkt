#lang racket/base

(require racket/string)

(struct board (rows index)
  #:transparent)

(define (read-board in)
  (cond
    [(eof-object? (read-line in)) eof]
    [else
     (define index (make-hasheqv))
     (define rows
       (for/vector ([i (in-range 5)])
         (define line (read-line in))
         (for/vector ([str (in-list (string-split line " " #:repeat? #t))]
                      [j (in-naturals)])
           (define num (string->number str))
           (begin0 num
             (hash-set! index num (cons i j))))))
     (board rows index)]))

(define (board-score b)
  (for/sum ([n (in-hash-keys (board-index b))])
    n))

(define (board-mark! b n)
  (define idx (board-index b))
  (when (hash-has-key? idx n)
    (define posn (hash-ref idx n))
    (define row (vector-ref (board-rows b) (car posn)))
    (vector-set! row (cdr posn) #f)
    (hash-remove! (board-index b) n)))

(define (board-complete? b)
  (define rows (board-rows b))
  (or (for/or ([r (in-vector (board-rows b))])
        (line-complete? r))
      (for/or ([j (in-range 5)])
        (line-complete?
         (for/vector ([i (in-range 5)])
           (vector-ref (vector-ref rows i) j))))))

(define (line-complete? l)
  (for/and ([n (in-vector l)])
    (not n)))

(define (read-data)
  (call-with-input-file "day04.txt"
    (lambda (in)
      (define numbers
        (map string->number (string-split (read-line in)  ",")))
      (define boards
        (for/list ([board (in-port read-board in)])
          board))
      (values numbers boards))))

(define part1
  (let-values ([(numbers boards) (read-data)])
    (for*/fold ([res #f])
               ([n (in-list numbers)]
                [b (in-list boards)])
      (board-mark! b n)
      #:final (board-complete? b)
      (* n (board-score b)))))

(define part2
  (let-values ([(numbers boards) (read-data)])
    (for*/fold ([res #f]
                [completed null]
                #:result res)
               ([n (in-list numbers)]
                [b (in-list boards)])
      (cond
        [(memq b completed)
         (values res completed)]
        [else
         (board-mark! b n)
         (if (board-complete? b)
             (values (* n (board-score b)) (cons b completed))
             (values res completed))]))))

(module+ test
  (require rackunit)
  (check-= part1 32844 0)
  (check-= part2 4920 0))
