#lang racket

(define (matched? m n)
  (match m
    [n #t]
    [else #f]))
    
   
  
         
(define (traverse s target before-exprs after-exprs)
  (cond
    [(syntax? s)
     ;(hash-set! pos-table (syntax-position s) s)
     
     (traverse (syntax-e s))]
    [(pair? s)
     (traverse (car s))
     (traverse (cdr s))]
    [else (void)]))