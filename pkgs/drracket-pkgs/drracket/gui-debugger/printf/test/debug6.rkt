#lang medic

(layer layer1
       (in #:file "src6.rkt"
           ;(with-behavior fact "return value is @,ret, y is @y")
           (with-behavior fact "function fact takes @x and returns @,ret")
           ;(with-behavior fact "function fact takes @x")
           [(fact) [on-entry 
                    (log x)
                    (log y)
                    
                    #;(printf "[debug]layer1 entered.\n")]]
           [on-exit (log (fact (+ 1 2) "yyy"))]))

(layer layer2
       (in #:file "src6.rkt"
           #;[on-exit
            (define (f) (log z))]
           [on-exit (log z)
                    (log (fact 5 "layer2"))]
           [(fact) [on-entry (log y)]]))
