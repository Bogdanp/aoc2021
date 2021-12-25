#lang racket/base

(#%declare #:unsafe)

(require racket/list
         racket/match
         racket/string)

(define prog
  (call-with-input-file "day24.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match (string-split line " ")
          [`(,instr ,reg)
           `(,(string->symbol instr) ,(string->symbol reg))]
          [`(,instr ,reg ,(app string->number imm)) #:when imm
           `(,(string->symbol instr) ,(string->symbol reg) ,imm)]
          [`(,instr ,dst ,src)
           `(,(string->symbol instr) ,(string->symbol dst) ,(string->symbol src))])))))

(define (replace expr sym e)
  (cond
    [(eq? expr sym) e]
    [(list? expr) (map (λ (subexpr) (replace subexpr sym e)) expr)]
    [else expr]))

(define (reduce e)
  (match e
    [`(fx+ 0 ,e) (reduce e)]
    [`(fx+ ,e 0) (reduce e)]
    [`(fx* 1 ,e) (reduce e)]
    [`(fx* ,e 1) (reduce e)]
    [`(fx* 0 ,e) 0]
    [`(fx* ,e 0) 0]
    [`(fxquotient ,e 1) (reduce e)]
    [`(set! ,dst (fxquotient ,dst 1)) #f]
    [`(set! ,dst ,dst-e) `(set! ,dst ,(reduce dst-e))]
    [_ e]))

(define (reduce* es)
  (filter-map reduce es))

(define (merge exprs)
  (define exprs* (reduce* exprs))
  (let loop ([expr (car exprs*)]
             [exprs* (cdr exprs*)]
             [folded null])
    (match-define `(set! ,dst ,body) expr)
    (match exprs*
      ['() (reverse (cons expr folded))]
      [(cons `(set! ,(== dst) ,next-body) exprs*)
       (loop (reduce `(set! ,dst ,(replace next-body dst body))) exprs* folded)]
      [(cons next exprs*)
       (loop next exprs* (cons expr folded))])))

(define (compile-prog)
  (define body
    (for/fold ([w-num 0]
               [w-id 'w_0]
               [body null]
               #:result (reverse body))
              ([instr (in-list prog)])
      (define (canonicalize reg)
        (case reg
          [(w) w-id]
          [else reg]))
      (define-values (new-w-num new-w-id expr)
        (match instr
          [`(inp ,_) (values (add1 w-num) (string->symbol (format "w_~a" w-num)) #f)]
          [_
           (values w-num w-id
                   (match instr
                     [`(add ,dst ,src) `(set! ,dst (fx+ ,dst ,(canonicalize src)))]
                     [`(mul ,dst ,src) `(set! ,dst (fx* ,dst ,(canonicalize src)))]
                     [`(div ,dst ,src) `(set! ,dst (fxquotient ,dst ,(canonicalize src)))]
                     [`(mod ,dst ,src) `(set! ,dst (fxmodulo ,dst ,(canonicalize src)))]
                     [`(eql ,dst ,src) `(set! ,dst (if (fx= ,dst ,(canonicalize src)) 1 0))]))]))
      (values new-w-num new-w-id (if expr (cons expr body) body))))

  `(define (f w)
     (define x 0)
     (define y 0)
     (define z 0)
     (define w_13 (modulo           w                 10))
     (define w_12 (modulo (quotient w 10)             10))
     (define w_11 (modulo (quotient w 100)            10))
     (define w_10 (modulo (quotient w 1000)           10))
     (define w_9  (modulo (quotient w 10000)          10))
     (define w_8  (modulo (quotient w 100000)         10))
     (define w_7  (modulo (quotient w 1000000)        10))
     (define w_6  (modulo (quotient w 10000000)       10))
     (define w_5  (modulo (quotient w 100000000)      10))
     (define w_4  (modulo (quotient w 1000000000)     10))
     (define w_3  (modulo (quotient w 10000000000)    10))
     (define w_2  (modulo (quotient w 100000000000)   10))
     (define w_1  (modulo (quotient w 1000000000000)  10))
     (define w_0  (modulo (quotient w 10000000000000) 10))
     (cond
       [(or (= w_13 0) (= w_12 0) (= w_11 0) (= w_10 0) (= w_9 0) (= w_8 0) (= w_7 0)
            (= w_6  0) (= w_5  0) (= w_4  0) (= w_3  0) (= w_2 0) (= w_1 0) (= w_0 0))
        +inf.0]
       [else
        ,@(merge body)
        z])))

(define compiled-prog (compile-prog))
(define f
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket/fixnum)
    (eval (compile compiled-prog))
    (namespace-variable-value 'f)))

(define (find-solution/p1 input [hi 11111111111111])
  (cond
    [(< input hi) #f]
    [else
     (define z (MONAD input))
     (cond
       [(zero? z) input]
       [else (find-solution/p1 (sub1 input) hi)])]))

;; find the greatest 8 digit prefix by hand then brute force it
(define part1
  (find-solution/p1 99911993999999 99911993111111))

(define (find-solution/p2 input [hi 99999999999999])
  (cond
    [(> input hi) #f]
    [else
     (define z (MONAD input))
     (cond
       [(zero? z) input]
       [else (find-solution/p2 (add1 input) hi)])]))

;; find the least 8 digit prefix by hand then brute force it
(define part2
  (find-solution/p2 62911941716111 62911941799999))

(module+ test
  (require rackunit)
  (check-= part1 99911993949684 0)
  (check-= part2 62911941716111 0))
