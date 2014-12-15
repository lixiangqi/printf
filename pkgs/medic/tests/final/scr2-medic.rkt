#lang medic

(layer layer1 
       (in #:file "src.rkt"
           ; match two instances of (inc-counter)
           [(at (inc-counter)) [on-entry (log "[1]calling inc-counter")]]
           
           ; match two instances of (+ x 1)
           [(at (+ x 1) [#:before (inc-counter)]) [on-entry (log "[2]calling (+ x 1)")]]
           
           ; only match (+ x 1) in g function
           [(at (+ x 1) [#:before (define x (inc 4))
                                  (inc-counter)])
            [on-entry (log "[3]calling (+ x 1) in g")]]
           [(g) [(at (+ x 1)) [on-entry (log "[4]match (+ x 1) in g")]]]
           
           ; only match (inc-counter) in function g
           [(at (inc-counter) [#:before (define x (inc 4))] [#:after (+ x 1)])
            (on-entry (log "[5]calling (inc-counter) in g"))]
           [(at (inc-counter) [#:before (with-start "(define x (inc")] [#:after (+ x 1)])
            (on-entry (log "[6]use with-start matching (inc-counter) in g"))]))
