#lang at-exp medic


#;(layer layer1
       (in #:file "src7.rkt"
           [on-entry
            (define test%
              (class object%
                (super-new)))
            (define a (new test%))
            (define b (new test%))
            (define c (new test%))
            (edge a b "ab")
            (edge b c "bc")]
           [on-exit (timeline r)
                    (timeline y)]
           [(fact) 
            [(at (with-start |(* x (fact (sub1 x|))
             [on-entry (timeline t)]]
            [on-entry (timeline x)
                      (assert (> x 0))]]))

(layer layer1
       (in #:file "src7.rkt"
           [(at (with-start "(* x (fact (sub1 x")) [on-exit (void)]]
           [(fact)
            [(at (with-start "(* x (fact (sub1 x"))
             [on-entry (timeline t)]]]))