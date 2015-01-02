#lang racket

(define a (syntax->datum #'(* x (sub1 y))))

(define b (syntax->datum #'r))
(define b1 (syntax->datum #'(* x (sub1 y))))

(define c (syntax->datum #'(* x (sub1 _))))

(define d (syntax->datum #'(* x _)))

;(match '(* (add1 x) (sub1 y))
;  [(list * b _)
;   (pair? b)])

(define (match? a b)
  (define res
  (if (and (syntax? a) (syntax? b))
      (let ([a-datum (syntax->datum a)]
            [b-datum (syntax->datum b)])
        (or (equal? b-datum '_)
            (equal? a-datum b-datum)))
      #f))
  (printf "match: a=~v, b=~v, res=~v\n" a b res)
  res)
  
(define (traverse m n)
  (printf "\nm=~v, n=~v\n" m n)
  (cond
    [(syntax? m)
     (if (match? m n)
         #t
         (traverse (syntax-e m) (syntax-e n)))]
    [(pair? m)
     ;(printf "first: m=~v, n=~v\n" (car m) (car n))
     ;(printf "second: m=~v, n=~v\n" (cdr m) (cdr n))
     (if (match? (car m) (car n))
         (traverse (cdr m) (cdr n))
         (and (traverse (car m) (car n))
              (traverse (cdr m) (cdr n))))
    
         ]
    [else (printf "else: m=~v, n=~v\n" m n)
          (equal? m n)]))

;(traverse #'(* x (sub1 y)) #'(* x (sub1 y)))
;(traverse #'(* x (sub1 y)) #'(* x (sub1 z)))
(traverse #'(* x (sub1 y)) #'(_ _ _))
