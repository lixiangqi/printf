#lang medic

(layer layer1
       (in #:file "src.rkt"
          
           
           [on-exit 
            (define test% (class object% 
                            (super-new) 
                            (define a 1)
                            (define/public (get-a) a)
                            (define/public (set-a) (set! a 2))))
            (define m (new test%))
            (define n (new test%))
            
          
            (define p (cons 1 m))
            (same? p)
            (same? m)
            (send m set-a)
            (same? m)
            ;(set! t (posn 1 2))
            (set! p (cons 1 m))
            (same? p)
;            (define s (cons 1 2))
;            (changed? s)
;            (set! t (cons 2 3))
;            (changed? t)
;            (set! t (cons 1 2))
;            (changed? t)
            
            ]))