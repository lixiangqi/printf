#lang medic

(layer layer1
       (in #:file "src6.rkt"
           ;(with-behavior fact "return value is @,ret, y is @y")
           ;(with-behavior fact "function fact takes @x and returns @,ret")
           ;(with-behavior fact "function fact takes @x")
           [(fact) [on-entry 
                    (log x)
                    (log y)
                    (aggregate x)
                    (aggregate x y)
                    (timeline x)
                    ;(timeline x)
                    (printf "[debug]layer1 entered.\n")]]
           [on-exit (log (fact (+ 1 2) "yyy"))]))

(layer layer2
       (in #:file "src6.rkt"
           [on-exit
            (define (f) 
              (log z)
              (timeline y)
              (timeline z))]
           [on-exit (log z)
                    (log y)]
           [(fact) [on-entry
                    (log z)
                    (log x)]]))

