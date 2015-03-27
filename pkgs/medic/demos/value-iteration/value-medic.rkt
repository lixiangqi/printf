#lang medic

(layer layer1
       (in #:module "value.rkt"
           [(at (let loop ([i 1]) _))
            [on-entry 
             (same? v)
             ]]
           [(at (loop (add1 i)))
            [on-entry 
             (define c (cons 1 2))
             (same? c)
             (set! c (cons 1 2))
             (same? c)
             (aggregate gamma (hash-ref v 'a) (hash-ref v 'e))]]))