#lang medic

(layer layer1
       (in #:file "src.rkt"
           [on-entry (define t 9) (timeline t)]
           [(fact) 
            [on-entry (timeline x)]
            [on-entry (changed? x)]]
           
           [on-exit 
            (assert (> x 0))
            (changed? x)
            (set! x 'a)
            (changed? x)
            
;            (define t (cons 1 2))
;            (changed? t)
;            (set! t (cons 2 3))
;            (changed? t)
            
;            (define s (cons 1 2))
;            (changed? s)
;            (set! t (cons 2 3))
;            (changed? t)
;            (set! t (cons 1 2))
;            (changed? t)
            
            ]))