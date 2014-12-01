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
           [on-entry
            (define (g) (void))
            (define test%
              (class object%
                (super-new)))
            (define a (new test%))
            (define b (new test%))
            (define c (new test%))
            (define d (new test%))
            (define e (new test%))
            (define f (new test%))
            (edge a b "ab" "" "")
            (edge b c "bc" "" "")
            (edge b d "bd" "" "")
            (edge b e "be" "" "")
            (edge b f "bf" "" "")]
           [on-exit (log z)
                    (aggregate y z)
                    (log (fact 7 "layer2"))]
           [(fact) [on-entry (log y)]]))
