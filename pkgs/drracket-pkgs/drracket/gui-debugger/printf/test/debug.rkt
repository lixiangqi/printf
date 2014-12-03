#lang medic

(layer layer1
       (in #:file "src.rkt"
           
           [on-exit 
            (define t (cons 1 2))
            (changed? t)
            (set! t (cons 2 3))
            (changed? t)
            (set! t (cons 2 3))
            (changed? t)
            (set! t (cons 1 2))
            (changed? t)]))