#lang medic

(layer layer1
       (in #:file "src6.rkt"
           (: fact "function fact takes @x and returns @,ret")
           [on-exit
            (define test%
              (class object%
                (super-new)))
            (define a (new test%))
            (define b (new test%))
            (define c (new test%))
            (edge a b #:edge-label "ab")
            (edge b c #:edge-label "bc")
            #;(get-graph-table)]
           ;[on-exit '(log (#%plain-app fact (quote 5)))]
           ;[on-exit (logg y)]
           [(fact) [on-entry 
                    (printf "[debug]layer1 entered.\n")]]))