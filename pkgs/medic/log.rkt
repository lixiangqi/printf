#lang racket

(provide logg)

#;(define-syntax (log stx)
  (syntax-case stx ()
    [(_ id) #'(printf "log x=~v\n" id)]))

(define (logg x)
  (void)
  #;(printf "entered!!!!!\n"))