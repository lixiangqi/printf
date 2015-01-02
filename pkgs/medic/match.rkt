#lang racket

(define a (syntax->datum #'(* x (sub1 y))))

(define b (syntax->datum #'r))
(define b1 (syntax->datum #'(* x (sub1 y))))

(define c (syntax->datum #'(* x (sub1 _))))

(define d (syntax->datum #'(* x _)))

(define (match? m n)
  (define (stx-equal? a b)
  (if (and (syntax? a) (syntax? b))
      (let ([a-datum (syntax->datum a)]
            [b-datum (syntax->datum b)])
        (or (equal? b-datum '_)
            (equal? a-datum b-datum)))
      #f))
  (cond
    [(syntax? m)
     (if (stx-equal? m n)
         #t
         (match? (syntax-e m) (syntax-e n)))]
    [(pair? m)
     (if (stx-equal? (car m) (car n))
         (match? (cdr m) (cdr n))
         (and (match? (car m) (car n))
              (match? (cdr m) (cdr n))))]
    [else (equal? m n)]))

(match? #'(* x (sub1 y)) #'(* x (sub1 y)))
(match? #'(* x (sub1 y)) #'(* x (sub1 z)))
(match? #'(* x (sub1 y)) #'(_ _ _))
(match? #'(* x (sub1 y)) #'_)
