#lang medic

(layer layer1
       (in #:file "src6.rkt"
           ;(with-behavior fact "return value is @,ret, y is @y")
           (with-behavior fact "function fact takes @x and returns @,ret")
           ;(with-behavior fact "function fact takes @x")
           [(fact) [on-entry 
                    (aggregate x y)
                    (log x)
                    (log y)]]
           [on-exit (log (fact (+ 1 2) "layer1"))]))

(layer layer2
       (in #:file "src6.rkt"
           [on-exit (log z)
                    (aggregate y z)
                    (log (fact 7 "layer2"))]
           [(fact) [on-entry (log y)]]))
